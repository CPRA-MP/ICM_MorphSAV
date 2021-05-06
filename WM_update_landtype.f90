subroutine update_landtype
    ! global arrays updated by subroutine:
    !      dem_lndtyp

    ! Subroutine that updates land type raster at end of year.
    !            
    ! dem_lndtyp : Land type classification of DEM pixel
    !               1 = vegetated wetland
    !               2 = water
    !               3 = unvegetated wetland/new subaerial unvegetated mudflat (e.g., bare ground)
    !               4 = developed land/upland/etc. that are not modeled in ICM-LAVegMod
    !               5 = flotant marsh
    ! 
    ! lnd_change_flag: Flag indicating why a pixel changed land type classification during the year
    !               -1 = conversion from vegetated wetland to open water due to inundation
    !               -2 = conversion from flotant marsh mat to open water                  
    !               -3 = conversion from marsh edge to open water due to erosion
    !                0 = no change
    !                1 = conversion from open water to land eligible for vegetation
    
    use params
    implicit none
    
    ! local variables
    integer :: i                                                    ! iterator
    integer :: c                                                    ! local compartment ID variable
    integer :: g                                                    ! local grid cell ID variable
    integer :: er                                                   ! local ecoregion ID variable
    
    write(  *,*) ' - updating landtype at end of current year'
    write(000,*) ' - updating landtype at end of current year'

    do i = 1,ndem
        if (dem_pldr(i) == 0) then                                  ! only update landtype for pixels outside of polders
            if (dem_to_bidem(i) == dem_NoDataVal) then              ! if pixel is not within ICM-BI-DEM sub-domain, use land change flag to update landscape
                if (lnd_change_flag(i) == 1) then                   ! convert water to new land mudflat eligible for vegetation establishment next year
                    dem_lndtyp(i) = 3
                else if (lnd_change_flag(i) == -1) then             ! convert inundated vegetated wetland to open water
                    dem_lndtyp(i) = 2
                else if (lnd_change_flag(i) == -2) then             ! convert dead flotant marsh to open water
                    dem_lndtyp(i) = 2
                else if (lnd_change_flag(i) == -3) then             ! convert eroded marsh edge to open water
                    dem_lndtyp(i) = 2
                end if
            else                                                    ! if pixel is in ICM-BI-DEM
                c = dem_comp(i)
                if (c /= dem_NoDataVal) then
                    if(dem_z(i) > stg_av_yr(c)) then                ! check if pixel is higher than mean water level
                        dem_lndtyp(i) = 1                           ! if so, define barrier island pixel as land
                    else
                        dem_lndtyp(i) = 2                           ! otherwise, barrier island pixel is water
                    end if
                end if    
            end if
        end if
    end do
    
    return
end