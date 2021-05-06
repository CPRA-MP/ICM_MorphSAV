subroutine new_subaerial_land
    ! global arrays updated by subroutine:
    !      lnd_change_flg
    !
    ! Subroutine determines whether new elevation has resulted in any pixels that are newly subaerial (e.g. above mean water level)
    
   
    use params
    implicit none
    
    ! local variables
    integer :: i                                                    ! iterator
    integer :: c                                                    ! local compartment ID variable
    integer :: g                                                    ! local grid cell ID variable
    real(sp) :: dep_yr                                              ! local variable for inundation depth over DEM pixel from current year mean stage
    real(sp) :: dep_prev_yr                                         ! local variable for inundation depth over DEM pixel from previous year mean stage
  
    
    do i = 1,ndem
        if (dem_to_bidem(i) == dem_NoDataVal) then                      ! only proceed if pixel is not within barrier island domain
            ! set local copies of grid and compartment numbers
            c = dem_comp(i)
            if (c /= dem_NoDataVal) then
                g = dem_grid(i)
                if (g /= dem_NoDataVal) then
                
                    ! reset local variables to zero for loop
                    dep_yr = 0.0
                    dep_prev_yr = 0.0
                    
                    ! set local copies of depth and salinity variable for current year
                    dep_yr = dem_inun_dep(i,13)

                    ! check here to see if lnd_change_flag has been change by any other subroutines,
                    ! if so, skip ahead to not overwrite previously run land change functions (e.g. edge erosion)
                    ! this flag is added to any subroutine that alters lnd_change_flag so that the priority of land change
                    ! is purely a function of the order in which the subroutines are called in MAIN
                    
                    if (lnd_change_flag(i) == 0) then
                        ! if water - check if current year elevation is above MWL by depth threshold defining whether vegetation can establish
                        if (dem_lndtyp(i) == 2) then
                            if (dep_yr < ht_abv_mwl_est) then
                                dep_prev_yr = dem_inun_dep(i,14)
                                ! if both current year and previous year have elevation above threshold for establishment, convert water to land eligible for vegetation
                                if (dep_prev_yr < ht_abv_mwl_est) then
                                    lnd_change_flag(i) = 1                          ! lnd_change_flag = 1 for conversion from open water to land eligible for vegetation
                                end if
                            end if
                        
                        end if
                    end if
                end if
            end if
        end if
    end do        

        
    return

end