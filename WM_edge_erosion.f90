subroutine edge_erosion
    ! global arrays updated by subroutine:
    !      lnd_change_flag
    !
    ! Subroutine that determines whether an edge pixel will be eroded during the model year
    ! This subroutine reads the assigned marsh edge erosion rate (m/yr) for a given pixel and calculates
    ! the number of years needed to erode one DEM pixel.
    ! For example, a pixel with an input edge erosion rate of 15 m/yr will only be subjected to edge erosion processes once every 2 years.
    ! A pixel with an edge erosion rate of 5 m/yr will be subjected to edge erosion processes once every 6 years.
    !
    !
    !
    ! *************************
    ! *
    ! * This subroutine is only capable of removing one edge pixel in a given year.
    ! * And since partial years are always rounded up to the next year, if you have an edge erosion rate of more than half of the DEM pixel resolution
    ! * over time, there will be an under-representation of edge erosion.
    ! * For example, if a location has an erosion rate of 29 meters per year, this would require 1.03 years to erode one 30-m pixel. 
    ! * However, this code rounds that up to erode one edge pixel every 2 years.
    ! * Over a 50-year simulation, the historic erosion rate would have resulted in 1450 meters of erosion, 
    ! * whereas this subroutine would only remove 25 pixels, resulting in 750 meters of erosion.
    ! * If the historic rate was 16 m/yr - this would result in 800 m over 50 years, or again 750 meters of modeled erosion.
    ! *
    ! * This could be changed by simply rounding instead of using a ceiling function on the year calculation.
    
    ! *************************
    
    use params
    implicit none
    
    ! local variables
    integer :: i                                                                        ! iterator
    integer :: mee_yr_increment                                                         ! number of years necessary to erode one DEM pixel - function of  MEE rate & DEM resolution
    real(sp) :: yr_ratio                                                                ! elapsed model year divided by number of years needed for edge erosion - when this ratio is a whole number, the edge erosion occurs
    real(sp) :: dec                                                                     ! decimal portion of yr_ratio
    real(sp) :: meer                                                                    ! local variable for marsh edge erosion rate of pixel
    do i = 1,ndem
        meer = dem_meer(i)
        if (meer /= dem_NoDataVal) then
            if (meer > 0.0) then
                if (dem_edge(i) == 1) then                                              ! check if DEM pixel is edge
                    mee_yr_increment = ceiling(float(dem_res) / meer )                  ! calculate number of years needed to erode one DEM pixel for given rate; ceiling results in a pixel that needs 24.5 years to erode to be eroded during year 25
                    yr_ratio = float(elapsed_year) / float(mee_yr_increment)
                    dec = int(yr_ratio) - yr_ratio
                    if( dec == 0) then
                        lnd_change_flag(i) = -3                                         ! lnd_change_flag = -3 for conversion from marsh edge to open water due to erosion
                    end if
                end if
            end if
        end if
    end do
        
    return
end