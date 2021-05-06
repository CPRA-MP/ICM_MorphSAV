subroutine edge_delineation
    ! global arrays updated by subroutine:
    !      dem_edge
    !
    ! Subroutine that uses a moving window across entire map to determine which land pixels are adjacent to water
    ! and are therefore classified as an edge pixel.
    ! 
    ! This subroutine will examine the lndtyp classification for 9 pixels at a time - for the 8 pixels surrounding pixel d0.
    ! The moving window map looks like:
    !
    !   Index Map      dem_lndtyp      Land/Water    Land/Edge/Water      dem_edge
    !  ~~~~~~~~~~~     ~~~~~~~~~~~     ~~~~~~~~~~~     ~~~~~~~~~~~        ~~~~~~~~~~~
    !   d1  d2  d3      1   1   1       L   L   L       L   L   L          0   0   0
    !   d8  d0  d4      3   1   1       L   L   L       E   E   L          1   1   0
    !   d7  d6  d5      2   3   1       W   L   L       W   E   L          0   1   0
    !  ~~~~~~~~~~~     ~~~~~~~~~~~     ~~~~~~~~~~~     ~~~~~~~~~~~        ~~~~~~~~~~~
    !
    ! This subroutine ignores upland and flotant marsh values since it is assumed that these two land classifications
    ! are not subjected to edge erosional processes, nor will they receive sediment deposition - which are the two
    ! ICM-Morph model processes that utilize edge classification.
    ! Both vegetated and nonvegetated land will be eligible for classification as edge (dem_lndtyp = 1 or dem_lndtyp = 3)
        
    
    use params
    implicit none
    
    ! local variables
    integer :: ic                                                       ! iterator over DEM columns
    integer :: ir                                                       ! iterator over DEM columns
    integer :: d0,d1,d2,d3,d4,d5,d6,d7,d8                               ! DEM index values used in 9-pixel moving window

    write(  *,*) ' - identifying marsh edge pixels'
    write(000,*) ' - identifying marsh edge pixels'
    
    ! initialize edge flag for each DEM pixel to zero
    dem_edge = 0
    
    ! loop over mapped array of pixel index values
    ! this will not allow any pixel on the outside border of the raster be classified as edge
    do ic = 2,n_dem_col-1
        do ir = 2,n_dem_row-1
            d0 = dem_index_mapped(ic, ir)
            if (dem_lndtyp(d0) /= dem_NoDataVal) then                   ! check if dem_lndtyp is assigned - if not, pixel is skipped
                if (dem_lndtyp(d0) < 4) then                            ! if not upland (dem_lntyp=4) nor flotant (dem_lntyp=5)
                    if (dem_lndtyp(d0) /= 2) then                       ! and if not water - then pixel is vegetated or nonvegetated wetland (dem_lndtype= 1 & 3, respectively)
                        d1 = dem_index_mapped(ic-1, ir+1)               ! grab DEM pixel index for first neighbor, d1
                        if (dem_lndtyp(d1) == 2) then                   ! if neighbor, d1, is water then 
                            dem_edge(d0) = 1                            ! set edge value for d0 to 1
                        else                                            ! if first neighbor is not water, check second neighbor, d2 and so on.
                            d2 = dem_index_mapped(ic  , ir+1)           ! use of else statements will stop the moving window at the first instance where d0 is set to edge
                            if(dem_lndtyp(d2) == 2) then
                                dem_edge(d0) = 1
                            else                                        
                                d3 = dem_index_mapped(ic+1, ir  )
                                if(dem_lndtyp(d3) == 2) then
                                    dem_edge(d0) = 1
                                else
                                    d4 = dem_index_mapped(ic+1, ir  )
                                    if(dem_lndtyp(d4) == 2) then
                                        dem_edge(d0) = 1
                                    else
                                        d5 = dem_index_mapped(ic+1, ir-1)
                                        if(dem_lndtyp(d5) == 2) then
                                            dem_edge(d0) = 1
                                        else
                                            d6 = dem_index_mapped(ic  , ir-1)
                                            if(dem_lndtyp(d6) == 2) then
                                                dem_edge(d0) = 1
                                            else
                                                d7 = dem_index_mapped(ic-1, ir-1)
                                                if(dem_lndtyp(d7) == 2) then
                                                    dem_edge(d0) = 1
                                                else
                                                    d8 = dem_index_mapped(ic-1, ir  )
                                                    if(dem_lndtyp(d8) == 2) then
                                                        dem_edge(d0) = 1
                                                    end if
                                                end if
                                            end if
                                        end if
                                    end if
                                end if
                            end if
                        end if
                    end if    
                end if        
            end if           
        end do 
    end do
    
    return
end