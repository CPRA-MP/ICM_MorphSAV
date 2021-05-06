subroutine flotant
    ! global arrays updated by subroutine:
    !      grid_pct_flt
    !      lnd_change_flg
    !
    ! Subroutine that updates lnd_change_flag array for pixels that are initially flotant marsh
    ! and also has dead flotant marsh in the ICM-LAVegMod grid cell for the year.
    ! 
    ! Subroutine also updates grid cell percent coverage of flotant marsh values
    ! based on input dead_flotant from ICM-LAVegMod output file.
    !
    ! This subroutine depends on the initial classification of a 30-m DEM pixel as flotant marsh
    ! and the amount of flotant marsh that was converted to open water within each ICM-LAVegMod
    ! grid cell, as reported out by ICM-LAVegMod as 'dead flotant'
    !
    ! This conversion from flotant to open water will be ordered in the same order as the input landtype DEM file.
    !
    !
    !
    !
    ! *************************
    ! *
    ! * This subroutine could be optimized by having the second loop from 1,ndem loop over the ngrid instead of ndem.
    ! * However, this would require a new 2-d array that has a list of all DEM pixels associated with each grid cell.
    ! * If updated, then looping over grid cells, you could limit the number of DEM pixels needed to be looped over by 
    ! * only initiating the pixel-level loops for grid cells that have a non-zero Dead Flotant value for the year.
    ! *
    ! *************************
    

    use params
    implicit none
    
    ! local variables
    integer :: i                                                    ! iterator
    integer :: g                                                    ! local grid ID variable
    integer,dimension(:),allocatable :: grid_flt_all                ! sum of flotant marsh pixels in each ICM-LAVegMod grid cell
    integer,dimension(:),allocatable :: grid_dead_flt_all           ! sum of dead flotant marsh pixels in each ICM-LAVegMod grid cell that will convert to open water
    integer,dimension(:),allocatable :: grid_dead_flt_killed_cntr   ! counter for each grid cell to track how many flotant pixels have been converted to water
    
    allocate(grid_flt_all(ngrid))
    allocate(grid_dead_flt_all(ngrid))
    allocate(grid_dead_flt_killed_cntr(ngrid))
    
    write(  *,*) ' - updating flotant marsh pixels'
    write(000,*) ' - updating flotant marsh pixels'
    ! count starting number of flotant marsh pixels in each ICM-LAVegMod grid cell
    grid_flt_all = 0
    do i = 1,ndem
        g = dem_grid(i)
        if (dem_lndtyp(i) == 5) then                                ! check if DEM pixel is classified as flotant marsh in input landscape
            grid_flt_all(g) = grid_flt_all(g) + 1    
        end if
    end do
    
    ! count number of dead flotant marsh pixels in each ICM-LAVegMod grid cell
    grid_dead_flt_all = 0
    do i = 1,ngrid
        if (grid_pct_dead_flt(i) > 0.0) then                        ! if there's dead flotant in grid cell
            if (grid_flt_all(i) > 0) then                           ! and there's flotant marsh to remove in grid cell
                grid_dead_flt_all(i) = int( grid_pct_dead_flt(i)*grid_flt_all(i) )
            end if
        end if
    end do
    
    ! loop through flotant marsh pixels and convert to open water
    ! update counter for each grid cell so that once percent dead floatant is met no more flotant pixels will be converted to water
    ! reduce count of flt pixels in grid counter
    grid_dead_flt_killed_cntr = 0
    do i = 1,ndem
        if (dem_lndtyp(i) == 5) then                                ! dem_lndtyp = 5 for flotant marsh
            g = dem_grid(i)
            if (g /= dem_NoDataVal) then
                if (grid_dead_flt_killed_cntr(g) < grid_dead_flt_all(g)) then
                    
                    lnd_change_flag(i) = -2                             ! lnd_change_flag = -2 for conversion from flotant marsh mat to open water
                    
                    grid_dead_flt_killed_cntr(g) = grid_dead_flt_killed_cntr(g) + 1
                    grid_flt_all(g) = max(0,grid_flt_all(g)-1)
                end if
            end if
        end if
    end do
    
    ! update percentage of ICM-LAVegMod grid cell that is still floating mat at end of year
    grid_pct_flt = grid_flt_all / max(grid_ndem_all,1)
    
    return

end