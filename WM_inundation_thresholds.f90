subroutine inundation_thresholds
    ! global arrays updated by subroutine:
    !      lnd_change_flg
    !      grid_pct_upland_wet
    !      grid_pct_upland_dry
    !
    ! Subroutine determines inundation stressors as functions of salinity and depth for two years in a row.
    !
    ! Subroutine also determines whether, for the current year, an upland pixel is inundated enough to be eligible for wetland vegetation.
   
    use params
    implicit none
    
    ! local variables
    integer :: i                                                    ! iterator
    integer :: c                                                    ! local compartment ID variable
    integer :: g                                                    ! local grid cell ID variable
    real(sp) :: dep_yr                                              ! local variable for inundation depth over DEM pixel from current year mean stage
    real(sp) :: sal_yr                                              ! local variable for salinity at DEM pixel from current year mean salinity
    real(sp) :: dep_prev_yr                                         ! local variable for inundation depth over DEM pixel from previous year mean stage
    real(sp) :: sal_prev_yr                                         ! local variable for salinity at DEM pixel from previous year mean salinity
    real(sp) :: MuDepth                                             ! mean depth for given salinity value from quantile regression - for current year  (see App. A of MP2023 Wetland Vegetation Model Improvement report)
    real(sp) :: SigmaDepth                                          ! st dev depth for given salinity value from quantile regression - for current year  (see App. A of MP2023 Wetland Vegetation Model Improvement report)
    real(sp) :: DepthThreshold_Wet                                  ! too wet for vegetation - depth threshold for given Z and given salinity value from quantile regression - for current year
    real(sp) :: DepthThreshold_Dry                                  ! too dry for vegetation - depth threshold for given Z and given salinity value from quantile regression - for current year
    real(sp) :: MuDepth_prv                                         ! mean depth for given salinity value from quantile regression - for previous year
    real(sp) :: SigmaDepth_prv                                      ! st dev depth for given salinity value from quantile regression - for previous year      
    real(sp) :: DepthThreshold_Wet_prv                              ! too wet for vegetation - depth threshold for given Z and given salinity value from quantile regression - for previous year
    integer,dimension(:),allocatable :: grid_n_upland_wet           ! local count of number of upland pixels in ICM-LAVegMod grid cell that are wet enough for wetland vegetation
    integer,dimension(:),allocatable :: grid_n_upland_dry           ! local count of number of upland pixels in ICM-LAVegMod grid cell that too dry for wetland vegetation
    
    allocate(grid_n_upland_wet(ngrid))
    allocate(grid_n_upland_dry(ngrid))

    
    ! initialize upland wet/dry counting arrays to zero
    grid_n_upland_wet = 0
    grid_n_upland_dry = 0
    
    do i = 1,ndem
        if (dem_to_bidem(i) == dem_NoDataVal) then                      ! only proceed if pixel is not within barrier island domain
            ! set local copies of grid and compartment numbers
            c = dem_comp(i)
            if (c /= dem_NoDataVal) then
                g = dem_grid(i)
                if (g /= dem_NoDataVal) then
                
                    ! reset local variables to zero for loop
                    MuDepth = 0.0
                    SigmaDepth = 0.0
                    DepthThreshold_Wet = 0.0
                    DepthThreshold_Dry = 0.0 
                    dep_yr = 0.0
                    sal_yr = 0.0
                    MuDepth_prv = 0.0
                    SigmaDepth_prv = 0.0
                    DepthThreshold_Wet_prv = 0.0
                    dep_prev_yr = 0.0
                    sal_prev_yr = 0.0  
                    
                    ! set local copies of depth and salinity variable for current year
                    dep_yr = dem_inun_dep(i,13)
                    sal_yr = sal_av_yr(c)

                    ! check here to see if lnd_change_flag has been change by any other subroutines,
                    ! if so, skip ahead to not overwrite previously run land change functions (e.g. edge erosion)
                    ! this flag is added to any subroutine that alters lnd_change_flag so that the priority of land change
                    ! is purely a function of the order in which the subroutines are called in MAIN
                    
                    if (lnd_change_flag(i) == 0) then
                        ! if vegetated land and not forested - check for inundation stress
                        if (dem_lndtyp(i) == 1) then
                            if ( grid_FIBS_score(g) > 0.15) then        ! Fresh forested areas have FFIBS <= 0.15
                                MuDepth = B0 + B1*sal_yr
                                SigmaDepth = B2 + B3*exp(B4*sal_yr)
                                DepthThreshold_Wet =  MuDepth  + ptile_Z*SigmaDepth
                                
                                ! if current year inundation is above threshold, check previous year            
                                if (dep_yr >= DepthThreshold_Wet) then
                                    dep_prev_yr = dem_inun_dep(i,14)
                                    sal_prev_yr = sal_av_prev_yr(c)
                                    
                                    MuDepth_prv = B0 + B1*sal_prev_yr
                                    SigmaDepth_prv = B2 + B3*exp(B4*sal_prev_yr)
                                    DepthThreshold_Wet_prv =  MuDepth_prv  + ptile_Z*SigmaDepth_prv
                                    
                                    ! if both current year and previous year have inundation above threshold, set flag to collapse land
                                    if (dep_prev_yr >= DepthThreshold_Wet_prv) then
                                        lnd_change_flag(i) = -1                         ! lnd_change_flag = -1 for conversion from vegetated wetland to open water
                                    end if
                                end if
                            end if

                        ! if upland - check if inundation would allow for wetland vegetation to establish
                        else if (dem_lndtyp(i) == 4) then
                            MuDepth = B0 + B1*sal_yr
                            SigmaDepth = B2 + B3*exp(B4*sal_yr)
                            DepthThreshold_Dry =  MuDepth - ptile_Z*SigmaDepth
                            
                            if (dep_yr >= DepthThreshold_Dry) then
                                grid_n_upland_wet(g) = grid_n_upland_wet(g) + 1 
                            else
                                grid_n_upland_dry(g) = grid_n_upland_dry(g) + 1
                            end if
                        
                        end if
                    end if
                end if
            end if
        end if
    end do        
    
    ! convert count of wet/dry upland pixels into percentage of grid cell
    grid_pct_upland_wet = 0.0
    grid_pct_upland_dry = 0.0

    grid_pct_upland_wet = float(grid_n_upland_wet)/max(0.001,float(grid_ndem_all))
    grid_pct_upland_dry = float(grid_n_upland_dry)/max(0.001,float(grid_ndem_all))
        
    return

end