subroutine update_elevation
    ! global arrays updated by subroutine:
    !      dem_z
    !      dem_dz_cm
    !      comp_sed_mass_loss
    !
    ! Subroutine updates elevation at each DEM pixel at end of year.
    ! Processes taken into account for dZ at each pixel depend on landtype classification.
    ! Additional elevation loss if land loss occurs at pixel
    !
    ! Subroutine also sums sediment mass of all land lost during the year within each ICM-Hydro compartment
    
    use params
    implicit none
    
    ! local variables
    integer :: i                                                        ! iterator
    integer :: c                                                        ! local compartment ID variable
    integer :: g                                                        ! local grid cell ID variable
    integer :: er                                                       ! local ecoregion ID variable
    real(sp) :: dz_cm                                                   ! change in elevation for DEM pixel during current year (cm)
    real(sp) :: dz_cm_lndtyp                                            ! land-type specific elevation change (e.g. lowering old bareground) (cm)
    
    write(  *,*) ' - updating elevation at end of current year'
    write(000,*) ' - updating elevation at end of current year'
    
    do i = 1,ndem
        dz_cm = 0.0
        dz_cm_lndtyp = 0.0
        
        if (dem_z(i) /= dem_NoDataVal) then
            c = dem_comp(i)
            if (c /= dem_NoDataVal) then
                if (dem_to_bidem(i) == dem_NoDataVal) then                                                                      ! if pixel is not within barrier island domain            
                    if (dem_bg_flag(i) == 1) then                                                                               ! lower old bareground pixels
                        dz_cm_lndtyp = -1.0*bg_lowerZ_m*100.0
                    else 
                        if (lnd_change_flag(i) == -2) then                                                                      ! lower dead flotant pixels to 1 m below mean water level
                            dz_cm_lndtyp = min(0.00,-100.0*( dem_z(i) - (stg_av_yr(c) - flt_lowerDepth_m) ))                    ! if current elevation is already more than 1-m below water level, do not allow for elevation gain
                        else if (lnd_change_flag(i) == -3) then                                                                 ! lower eroded edge pixels to  25 cm below mean water level
                            dz_cm_lndtyp = min(0.00,-100.0*( dem_z(i) - (stg_av_yr(c) - me_lowerDepth_m) )) 
                        end if
                    end if
                
                    er = comp_eco(c)            
                    if (er /= dem_NoDataVal) then
                        if (dem_lndtyp(i) == 1) then
                            if (dem_pldr(i) == 1) then                                                                          ! poldered vegetated land dz_cm = f(deep)
                                dz_cm = 0.0 - dem_dpsb(i)/10.0 + dz_cm_lndtyp                                                   ! convert subsidence from mm/yr to cm/yr
                            elseif (lnd_change_flag(i) == 0) then                                                                  ! non-polder vegetated land dz_cm = f(organic, mineral, deep, shallow),
                                dz_cm = org_accr_cm(i) + min_accr_cm(i) - dem_dpsb(i)/10.0 - er_shsb(er,ssub_col)/10.0 + dz_cm_lndtyp    ! convert subsidence from mm/yr to cm/yr
                            else
                                dz_cm = min_accr_cm(i) - dem_dpsb(i)/10.0 - er_shsb(er,ssub_col)/10.0 + dz_cm_lndtyp
                            end if
                        else if (dem_lndtyp(i) == 2) then                                                                       ! water bottom dz_cm = f(mineral, deep)
                            dz_cm = min_accr_cm(i) - dem_dpsb(i)/10.0 + dz_cm_lndtyp                                            ! convert subsidence from mm/yr to cm/yr
                        else if (dem_lndtyp(i) == 3) then                                                                       ! nonvegetated wetland dz_cm = f(mineral, deep, shallow)
                            dz_cm = min_accr_cm(i) - dem_dpsb(i)/10.0 - er_shsb(er,ssub_col)/10.0  + dz_cm_lndtyp                        ! convert subsidence from mm/yr to cm/yr
                        else if (dem_lndtyp(i) == 4) then                                                                       ! upland/developed dz_cm = f(deep)
                            dz_cm = 0.0 - dem_dpsb(i)/10.0 + dz_cm_lndtyp                                                       ! convert subsidence from mm/yr to cm/yr
                        else                                                                                                    ! flotant dz_cm = 0
                            dz_cm = 0.0
                        end if 
                    end if
                    
                    dem_z(i) = dem_z(i) + dz_cm/100.0                                                                           ! dz_cm is in cm, DEM is in meters
                    dem_dz_cm(i) = dz_cm                                                                                        ! update dZ array for writing output file
                
                else        
                    dem_z(i) = dem_z_bi(dem_to_bidem(i))                                                                        ! if in BI-DEM domain,, use ICM-BI-DEM elevation as final elevation
                end if
            end if
        end if
    end do
    


    
    return
end