subroutine sav
    ! global arrays/variables used by subroutine:
    !   comp_eco
    !   dem_dtl
    !   dfl_params
    !   grid_FIBS_score
    !   ndem
    !   prior_int
    !   prior_slope
    !   sal_av_mons
    !   spsal_params
    !   sptss_params
    !   tss_av_mons
    !
    !
    ! Subroutine that calculates the probability of occurrence and non-occurrence of submerged aquatic vegetation (SAV) within each LAVegMod grid cell.
    ! The probability of occurence is converted to presence/absence based on the relative probability of occurrence/non-occurrence of SAV.
    ! The statistical analysis was conducted by the USGS (DeMarco et al., 2021) and is documented as a standalone report for the 2023 MP.
    ! The statistical model uses the following variables:
    !   - mean springtime salinity (ppt)
    !   - mean springtime total suspended sediment (mg/L)
    !   - distance to nearest land (m)
    !   - and a prior probability of SAV as a function of the vegetation type of the nearest land (calculated as a seperate relationship for each CWPPRA basin - mapped in the input file to 2023 MP ecoregion boundaries)
    ! The SAV statisical model is valid for any area that is less than 2000-m from land (beyond which there is too much exposure for SAV to be present - based on this model).
    ! The distance to land is calculated in the DISTANCE_TO_LAND subroutine for each DEM pixel (the search stops after the 2000-m limit is reached)
    ! The minimium distance to land value within each LAVegMod 
        
    
    use params
    implicit none
    
    
    ! local variables
    real(sp),parameter :: pi = 3.1415927
    integer :: i                                                        ! iterator over DEM pixels
    integer :: ig                                                       ! iterator over LAVegMod grid cells
    integer :: c                                                        ! local copy of Hydro compartment number
    integer :: g                                                        ! local copy of LAVegMod grid cell
    integer :: en                                                       ! local copy of ecoregion number for current LAVegMod grid cell
    integer :: pres                                                     ! presence (1) or absence (0) of SAV in ALVegMod grid cell (NoData = -9999)
    real(sp) :: ffibs                                                   ! local copy of FFIBS score for current LAVegMod grid cell
    real(sp) :: spsal                                                   ! local copy of mean spring salinity for current LAVegMod grid cell [ppt]
    real(sp) :: sptss                                                   ! local copy of mean spring TSS concentration for current LAVegMod grid cell [mg/L]
    real(sp) :: dfl                                                     ! local copy of distance to land for current LAVegMod grid cell [m]
    real(sp) :: ans1_sal_part                                           ! salinity predictor for probability grid cell <WILL> have SAV present
    real(sp) :: ans0_sal_part                                           ! salinity predictor for probability grid cell <WILL NOT> have SAV present
    real(sp) :: ans1_tss_part                                           ! suspended sediment predictor for probability grid cell <WILL> have SAV present
    real(sp) :: ans0_tss_part                                           ! suspended sediment predictor for probability grid cell <WILL NOT> have SAV present
    real(sp) :: ans1_dfl_part                                           ! distance-to-land predictor for probability grid cell <WILL> have SAV present
    real(sp) :: ans0_dfl_part                                           ! distance-to-land predictor for probability grid cell <WILL NOT> have SAV present
    real(sp) :: ans1                                                    ! combined predictor for probability grid cell <WILL> have SAV present
    real(sp) :: ans0                                                    ! combined predictor for probability grid cell <WILL> have SAV present
    real(sp) :: prior, pri, prs                                         ! prior probability, intercept, and slope for SAV based on location and marsh vegetation of nearest land
    real(sp) :: prob, prob_pres, prob_abs                               ! probability of SAV in grid cell, probability of presence, probability of absence
    real(sp),dimension(:),allocatable :: grid_dtl                       ! local array to store minimum distance to land for each LAVegMod grid cell
    real(sp),dimension(:),allocatable :: grid_dtl_ffibs                 ! local array to store FFIBS score for pixel that has minimum distance to land in each LAVegMod grid cell
    integer :: def_val
    
    !allocate(grid_dtl(ngrid))
    !allocate(grid_dtl_ffibs(ngrid))
    !grid_dtl = maxval(dem_dtl)                ! initialize the whole grid_dtl array to the maximum distance-to-land values found in the pixel-level distance-to-land array
    !grid_dtl_ffibs = -9999                    ! initialize the whole grid_dtl_ffibs to NoData
    
    write(  *,*) ' - calculating probability for SAV in each LAVegMod grid cell'
    write(000,*) ' - calculating probability for SAV in each LAVegMod grid cell'
    
    open(unit=8888, file = trim(adjustL(grid_sav_file))//'.csv')
    
    open(unit=8889, file = trim(adjustL(grid_sav_file))//'_prob.xyz')
    !open(unit=8887, file = trim(adjustL(grid_sav_file))//'prob_pres.xyz')
    !open(unit=8886, file = trim(adjustL(grid_sav_file))//'prob_abs.xyz')
    !open(unit=8885, file = trim(adjustL(grid_sav_file))//'spsal.xyz')
    !open(unit=8884, file = trim(adjustL(grid_sav_file))//'sptss.xyz')
    !open(unit=8883, file = trim(adjustL(grid_sav_file))//'dfl.xyz')
    
    !write(8888,'(A)') 'dem_x,dem_y,gridID,compID,spsal,ans1_sal_part,ans0_sal_part,sptss,ans1_tss_part,ans0_tss_part,dfl,ans1_dfl_part,ans0_dfl_part,ffibs,pri,prs,prior,ans1,ans0,pres,prob,prob_pres,prob_abs'
    write(8888,'(A)') 'dem_x,dem_y,gridID,compID,spsal,sptss,dfl,ffibs,prob'
    
    ! assign minimum distance-to-land found in each ICM-LAVegMod grid cell as well as the FFIBS score for the corresponding nearest land pixel
    do i=1,ndem
        ig = dem_grid(i)
        ! start calculations of prob of each predictor with (1) and without (0) SAV    
        ! initialize SAV variables to NoData before calculating so print outs account for NoData grid cells
        c = -9999
        en = -9999
        pres = -9999
        prob = -9999
 
        spsal = -9999
        sptss = -9999
        dfl = -9999
        ffibs = -9999
        
        ans1 = -9999 
        ans0 = -9999 
        prior = -9999 
        pri = -9999 
        prs = -9999 
        
        ans1_dfl_part = -9999 
        ans0_dfl_part = -9999 
        ans1_sal_part = -9999 
        ans0_sal_part = -9999 
        ans1_tss_part = -9999 
        ans0_tss_part = -9999 
        if (ig > 0) then   
            c = grid_comp(ig)
        end if
        if (c > 0) then
            en = comp_eco(c)
            if (en > 0) then
                dfl = dem_dtl(i)
                ffibs = dem_dtl_ffibs(i)
                if (ffibs >= 0) then                ! some pixels that are the closest land may be located in a grid cell that has no FFIBS score (either due to flotant or developed land being present) - these areas will report out a SAV prob of -9999
                    if (dfl <= 0) then              ! grid cell is 100% land/marsh so SAV cannot occur
                        prob = 0.0
                        pres = 0
                        ans1 = 0.0
                        ans0 = 0.0
                        prior = 0.0
                    else if (dfl > 2010) then       ! grid cell is further than 2 km from land - too much exposure for SAV cannot occur
                        prob = 0.0
                        pres = 0
                        ans1 = 0.0
                        ans0 = 0.0
                        prior = 0.0 
                    else if (dfl <= 2010) then      ! grid cell has some water that is less than 2 km from nearest land - calculate SAV probability
                        spsal = max(0.1, ( sal_av_mons(c,3)+sal_av_mons(c,4)+sal_av_mons(c,5) ) / 3.0 )
                        sptss = max(1.0, ( tss_av_mons(c,3)+tss_av_mons(c,4)+tss_av_mons(c,5) ) / 3.0 )
                    
                        ! spring salinity SAV probability 
                        ans1_sal_part = 1/(spsal_params(3)*SQRT(2.0*pi))*EXP(-0.5*((LOG(spsal)-(spsal_params(1)+spsal_params(2)))/spsal_params(3))**2)
                        ans0_sal_part = 1/(spsal_params(3)*SQRT(2.0*pi))*EXP(-0.5*((LOG(spsal)-(spsal_params(1)))/spsal_params(3))**2)
                        
                        ! distance from land SAV probability
                        ans1_dfl_part = 1/(dfl_params(3)*SQRT(2.0*pi))*EXP(-0.5*((LOG(dfl)-(dfl_params(1)+dfl_params(2)))/dfl_params(3))**2)
                        ans0_dfl_part = 1/(dfl_params(3)*SQRT(2.0*pi))*EXP(-0.5*((LOG(dfl)-(dfl_params(1)))/dfl_params(3))**2)
                        
                        ! spring TSS SAV probability
                        ans1_tss_part = 1/(sptss_params(3)*SQRT(2.0*pi))*EXP(-0.5*((LOG(sptss)-(sptss_params(1)+sptss_params(2)))/sptss_params(3))**2)
                        ans0_tss_part = 1/(sptss_params(3)*SQRT(2.0*pi))*EXP(-0.5*((LOG(sptss)-(sptss_params(1)))/sptss_params(3))**2)
    
                        ! calculate prior probability from location/veg type
                        pri = prior_int(en)
                        prs = prior_slope(en)
                        prior = 1/(1+EXP(-(prior_int(en)+prior_slope(en)*ffibs)))
                        
                        ! combine info from predictors
                        ans1 = ans1_sal_part*ans1_dfl_part*ans1_tss_part
                        ans0 = ans0_sal_part*ans0_dfl_part*ans0_tss_part
                        
                        prob = prior*ans1/(prior*ans1+(1-prior)*ans0)
                        
                        ! convert probability of SAV to presence/absence
                        prob_pres = LOG( prior*ans1 )
                        prob_abs  = LOG( (1-prior)*ans0 )
                        if ( prob_pres > prob_abs ) then
                            pres = 1
                        else
                            pres = 0
                        end if
                    end if
                end if

                ! write CSV output for all pixels that are water and within the ecoregion/compartment/grid domain                    
                if ( dfl > 0) then
                    write(8888,9998) dem_x(i),dem_y(i),ig,c,spsal,sptss,dfl,ffibs,prob
                end if
                
                ! write full CSV file for de-bugging. This will save values for all pixels that are within an ecoregion/compartment/grid - will skip all pixels outside of those domains
                !write(8888,9990) dem_x(i),dem_y(i),ig,c,spsal,ans1_sal_part,ans0_sal_part,sptss,ans1_tss_part,ans0_tss_part,dfl,ans1_dfl_part,ans0_dfl_part,ffibs,pri,prs,prior,ans1,ans0,pres,prob,prob_pres,prob_abs
                
            end if
        end if
        
        ! write XYZ file for all pixels
        write(8889,1801) dem_x(i),dem_y(i),prob
        !write(8887,1801) dem_x(i),dem_y(i),prob_pres
        !write(8886,1801) dem_x(i),dem_y(i),prob_abs
        !write(8885,1801) dem_x(i),dem_y(i),spsal
        !write(8884,1801) dem_x(i),dem_y(i),sptss
        !write(8883,1801) dem_x(i),dem_y(i),dfl
   
    end do
    
    close(8888)
    close(8889)
    !close(8887)
    !close(8886)
    !close(8885)
    !close(8884)
    !close(8883)
    
9998 format( 4(I0,','), F0.4, 4(',',F0.4) )    
9990 format( 4(I0,','), F0.4, 18(',',F0.4) )    
9999 format( 2(I0,','), F0.4, 6(',',F0.4), 2(',',I0), 11(',',F0.4) )
1800 format(I0,2(4x,I0))
1801 format(2(I0,4x),F0.4)   
    return
end
