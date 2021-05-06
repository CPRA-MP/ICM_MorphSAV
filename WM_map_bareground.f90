subroutine map_bareground
    ! global arrays updated by subroutine:
    !      dem_lndtyp
    !      dem_bg_flag
    !
    ! Subroutine that maps bareground toe the lowest elevated land pixels within each ICM-LAVegMod grid cell
    ! This subroutine will first map old bareground, so the lowest land pixels will be old bareground.
    ! The new bareground is then mapped -  so that newbare ground is between old bareground and vegetated land.
    ! This algorithm is run before the inundation thresholds, so newly built subaerial mudflats are not considered here.
    ! Those are elevation-based criteria, so they do not need to have an elevation assigned.
    ! And, those pixels are coming from previously defined water pixels, and are therefore excluded from this analysis
    ! as long as this function is run before the inundation thresholds subroutine that identifies new mudflat areas.
    !
    !
    !
    !
    ! *************************
    ! *
    ! * This subroutine could be optimized by putting a sort algorithm (e.g., heapsort, mergesort, etc.) in the minimum elevation
    ! * check. Currently, each grid cell has an array with a list of pixel index values and an array with the respective elevation.
    ! * These elevation arrays are used to find the minval elevation of each array on a series of loops.
    ! * The array is looped over to find minval for the number bareground pixels in the grid cell.
    ! * Once an elevation has been identified as a minval on one loop, that pixel index is cataloged, and then the temporary elevation in the 
    ! * grid array is set to a max value that will not be identified as the minval in subsequent loops.
    ! * While this works, the array size never gets smaller as minvals are identified, they are just re-set to default high values.
    ! * A smarter sort algorithm could reduce the array as minvals are cataloged and ultimately reduce the number of iterations.
    ! * Regardless, these loops are set to stop as soon as enough pixels have been identified to meet the number required 
    ! * as defined by the percent bareground values in the grid cell. 
    ! * 
    ! * Also, the loops are never entered if a grid cell does not haveany bareground to start with.
    ! *
    ! *************************
    

    use params
    implicit none
    
    ! local variables
    integer :: i                                                                    ! iterator
    integer :: gi                                                                   ! iterator
    integer :: bg                                                                   ! iterator
    integer :: gin                                                                  ! iterator
    integer ::gc                                                                    ! counter that is updated to track index of land pixels in each grid cell
    integer :: g                                                                    ! local grid ID variable
    integer :: dem_i                                                                ! local DEM pixel index
    integer,dimension(:),allocatable :: grid_lnd_cntr                               ! local array to count number of land pixels per grid cell
    integer,dimension(:,:),allocatable :: grid_lnd_i                                ! local array to store DEM index for land pixels within each grid cell
    real(sp),dimension(:,:),allocatable :: grid_lnd_z                               ! local array to store DEM elevation for land pixels within each grid cell            
    integer :: grid_n_bare_old                                                      ! local number of pixels of old bareground in each grid cell (calculated from input percentages)
    integer :: grid_n_bare_new                                                      ! local number of pixels of new bareground in each grid cell (calculated from input percentages)
    
    allocate(grid_lnd_cntr(ngrid))
    allocate(grid_lnd_i(ngrid,grid_ndem_mx))
    allocate(grid_lnd_z(ngrid,grid_ndem_mx))
    
   
    write(  *,*) ' - mapping bare ground within each ICM-LAVegMod grid cell'
    write(000,*) ' - mapping bare ground within each ICM-LAVegMod grid cell'
    
    dem_bg_flag = 0                                                                 ! initialize bareground flag to 0 (0 = not bareground; 1 = old bareground; 2 = new bareground)
    
    ! first, convert any bare ground in initial lnd_type to vegetated land
    do i = 1,ndem
        if (dem_lndtyp(i) == 3) then
            dem_lndtyp(i) = 1    
        end if
    end do

    grid_lnd_cntr = 0                                                               ! initialize grid counter!
    grid_lnd_i =  0!-9999                                                           ! initialize grid pixel index to NoData
    grid_lnd_z = 9999                                                               ! initialize elev array to 9999 so NoData z is always greater

    ! loop through land pixels and append DEM index and elevation to an array for each grid cell
    do i = 1,ndem
        g = dem_grid(i)
        if (g /= dem_NoDataVal) then
            gc = grid_lnd_cntr(g)
            if (dem_lndtyp(i) == 1) then
                grid_lnd_cntr(g) = min(gc + 1,grid_ndem_mx)                         ! determine array location for grid cell to append index and elevation to
                grid_lnd_i(g,gc+1) = i                                              ! populate array with DEM pixel index for all land pixels in grid cell
                grid_lnd_z(g,gc+1) = dem_z(i)                                       ! populate array with DEM pixel elevation for all land pixels in grid cell
            end if
        end if
    end do
    
    
    
    ! Map old and new bareground to lowest elevation land pixels in each grid cell.
    ! Loop is only entered if the cell has bareground located in it.
    ! Loop will stop once all bareground in grid cell has been assigned an elevation and location.
    ! Elevation array is same size for every loop and the minval function operates on the entire array, 
    ! even if pixel's elevation has already been id'd as a minimum, the array is not truncated, rather
    ! the elevation is re-set to a high value so it won't be flagged as the minimum more than once.
    ! This could be optimized with a sorting function (e.g., heapsort) instead of this default high re-set.
    
    do gi = 1,ngrid                                                                 ! loop over grid cells
        if (grid_pct_bare_old(gi) > 0.0) then                                       ! if there is old bareground in grid cell
            grid_n_bare_old = int(grid_pct_bare_old(gi)*grid_ndem_all(gi))          ! set number of pixels in grid that are old bare ground from percentage
            do bg = 1,grid_n_bare_old                                               ! loop over the number of old bareground pixels in grid cell               
                do gin = 1, grid_lnd_cntr(gi)                                       ! loop over all land pixels in grid cell 
                    if (grid_lnd_z(gi,gin) == minval(grid_lnd_z(gi,:))) then        ! find the pixel with lowest elevation
                        grid_lnd_z(gi,gin) = 9999                                   ! set current elevation to NoData so it won't be a minval in next loop
                        dem_i = grid_lnd_i(gi,gin)                                  ! find DEM pixel index for lowest land pixel
                        dem_lndtyp(dem_i) = 3                                       ! set lndtype for pixel to bareground
                        dem_bg_flag(dem_i) = 1                                      ! set bareground flag for pixel to old bareground (1)
                    end if
                end do
            end do
        end if

        ! repeat above process for new bareground - this places new bareground higher than old bareground but lower than vegetated land
        if (grid_pct_bare_new(gi) > 0.0) then                                       ! if there is new bareground in grid cell
            grid_n_bare_old = int(grid_pct_bare_old(gi)*grid_ndem_all(gi))          ! set number of pixels in grid that are new bare ground from percentage
            do bg = 1,grid_n_bare_new                                               ! loop over the number of new bareground pixels in grid cell               
                do gin = 1, grid_lnd_cntr(gi)                                       ! loop over all land pixels in grid cell 
                    if (grid_lnd_z(gi,gin) == minval(grid_lnd_z(gi,:))) then        ! find the pixel with lowest elevation
                        grid_lnd_z(gi,gin) = 9999                                   ! set current elevation to NoData so it won't be a minval in next loop
                        dem_i = grid_lnd_i(gi,gin)                                  ! find DEM pixel index for lowest land pixel
                        dem_lndtyp(dem_i) = 3                                       ! set lndtype for pixel to bareground
                        dem_bg_flag(dem_i) = 2                                      ! set bareground flag for pixel to new bareground (2)
                    end if
                end do
            end do
        end if
    end do
        
    return

end