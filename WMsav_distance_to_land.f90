subroutine distance_to_land
    ! global arrays updated or used by subroutine:
    !       dem_dtl
    !       dem_index_mapped
    !       dem_lndtyp
    !       dem_NoDataVal
    !       dem_res
    !       ndem
    !
    ! Subroutine that uses a moving window across entire map to determine distance to nearest land pixel.
    ! The moving window search is limited to 2010 meters within each pixel due to the SAV function ignoring all
    ! distance-to-land values that are greater than 2000 meters.
    ! 
    ! This subroutine will examine the lndtyp classification concentrically surrounding d00. As soon as land is 
    ! found, the search is ended since it is only interested in the smallest distance-to-land. If the entire 
    ! search d01-d08 ends without any land, then the next concentric ring is searched, d09-d24. 
    !
    ! The moving window map looks like:
    !
    !  Moving Window Index Map              Moving Window column index                Moving window row index
    !  ~~~~~~~~~~~~~~~~~~~~~~~~~           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    !   d09  d10  d11  d12  d13             ic-2  ic-1  ic  ic+1  ic+2             ir+2  ir+2  ir+2  ir+2  ir+2
    !   d24  d01  d02  d03  d14             ic-2  ic-1  ic  ic+1  ic+2             ir+1  ir+1  ir+1  ir+1  ir+1 
    !   d23  d08  d00  d04  d15             ic-2  ic-1  ic  ic+1  ic+2              ir    ir    ir    ir    ir
    !   d22  d07  d06  d05  d16             ic-2  ic-1  ic  ic+1  ic+2             ir-1  ir-1  ir-1  ir-1  ir-1 
    !   d21  d20  d19  d18  d17             ic-2  ic-1  ic  ic+1  ic+2             ir-2  ir-2  ir-2  ir-2  ir-2
    !  ~~~~~~~~~~~~~~~~~~~~~~~~~           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    ! This subroutine treats all non-water landtype classifications as land (e.g. marsh, upland, flotant).
    ! The statistical model for this SAV model was developed by DeMarco et al., 2021 (2023 MP documentation).

    use params
    implicit none
    

    
    ! local variables
    integer :: ws                                                               ! size of moving window (number of pixels to look in each direction around central pixel)
    integer :: ic                                                               ! iterator over DEM columns
    integer :: ir                                                               ! iterator over DEM columns
    integer :: d00                                                              ! DEM pixel index of current pixel being examined
    integer :: dxx                                                              ! DEM pixel in current concentric ring being examined, relative to d00
    integer :: rc                                                               ! local counter for columns in current concentric ring
    integer :: rr                                                               ! local counter for rows in current concentric ring
    integer :: ring                                                             ! counter of concentric ring being looped over (max ring = ws)
    integer :: land_ho                                                          ! flag set to 1 when land is found in current concentric ring being looped over
    real(sp) :: local_dtl                                                       ! local distance to land for pixel d00 when land is found at pixel dxx
    real(sp) :: ring_dtl_mn                                                     ! minimum distance-to-land for the current concentric ring
    integer ::  pn                                                              ! iterator
    integer :: nprogress                                                        ! progress of looping in pixel count

    ws = 5 !67                                                                     ! maximum distance (in DEM pixels) to search for land
    
    write(  *,'(A,I6,A)') ' - determining distance to land for each DEM pixel (search limited to ',ws*dem_res,' m)'
    write(000,'(A,I6,A)') ' - determining distance to land for each DEM pixel (search limited to ',ws*dem_res,' m)'
    
    !dem_dtl = 500.
    dem_dtl = ws*dem_res+1                                                      ! initialize distance to land for each DEM pixel to maximum distance-to-land value
                                                                                ! if ws is 67 and dem_res is 30, than the maximum distance searched will be 2010
                                                                                ! therefore, if the entire search window was examined and no land was found, 
                                                                                ! the distance to land will be set equal to 2011 (e.g. more than 2010 meters to land)
    nprogress = 0
    
    do ic = ws+1,n_dem_col-ws-1                                                 ! loop over mapped array of pixel index values
        do ir = ws+1,n_dem_row-ws-1                                             ! this will not allow any pixel on the outside border of the raster be included in search
            
            nprogress = nprogress + 1
            if (nprogress == 100000) then
                write(*,*) 'finished checking 100k pixels'
                nprogress=0
            end if
           
            d00 = dem_index_mapped(ic, ir)                                      ! current pixel being calculated is d00 with grid coordinates of (ic,ir)
            
            if (dem_lndtyp(d00) /= dem_NoDataVal) then                          ! check if dem_lndtyp is assigned - if not, pixel is skipped
                if (dem_lndtyp(d00) /= 2) then                                  ! check if dem_lndtyp is water, if not water, set distance to land to zero - and move on to next pixel
                    dem_dtl(d00) = 0
                else                                                            ! pixel d00 is water, so start looking for land
                    do ring = 1,ws                                              ! start search concentrically moving outward from d00 one ring at a time until max windowsize is reached
                        ring_dtl_mn = ws*dem_res+1                              ! initialize the minimum distance to land for the current search ring to maximum search distance+1 meter
                        land_ho = 0                                             ! set flag to 0 for start of ring
                        do rc = -ring,ring                                      ! local counter for columns in current concentric ring
                            do rr = -ring,ring                                  ! local counter for rows in current concentric ring
                                dxx = dem_index_mapped(ic+rc, ir+rr)            ! pixel in current concentric ring with grid coordinates of (ic+rc,ir+rr)
                                if (dem_lndtyp(dxx) /= 2) then                  ! LAND HO !!!
                                    write(*,*) 'land ho! land ho! land at ',ic, ir
                                    land_ho = 1
                                    local_dtl = sqrt( float((rc*dem_res)**2) + float((rr*dem_res)**2) ) ! calculated distance to land for current pixel where land was found (this is only first land pixel found in current ring - there may be a closer one later in the same ring, so keep looping over current ring)
                                    ring_dtl_mn = min(ring_dtl_mn, local_dtl)                           ! update minimum distance to land for current ring
                                    dem_dtl(ic) = ring_dtl_mn                                           ! update the distance to land value for the current DEM pixel to the current minimum
                                end if
                            end do
                        end do
                        
                        if (land_ho == 1) then                                  ! finished searching over the current concentric ring - break do loop if land was found on ring
                            exit
                        end if
                        
                    end do
                end if
            end if
        end do
    end do

    return
end
