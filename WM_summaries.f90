subroutine summaries
    ! global arrays updated by subroutine:
    !   grid_pct_water
    !   grid_pct_bare
    !   grid_pct_upland
    !   grid_pct_flt
    !   grid_pct_edge
    !   grid_bed_z
    !   grid_land_z     
    !   comp_pct_water 
    !   comp_pct_upland
    !   comp_edge_area
    !   comp_water_z
    !   comp_wetland_z
    
    !  subroutine that summarizes landscape area and average elevation for ICM-LAVegMod grid cells and ICM-Hydro compartments
    
    
    use params
    implicit none
    
    ! local variables
    integer :: i, ig, ic                                            ! iterator
    integer :: c                                                    ! local compartment ID variable
    integer :: g                                                    ! local grid ID variable
    integer :: lt                                                   ! local copy of landtype classification of pixel
    integer :: z                                                    ! local copy of elevation of pixel
    integer :: e                                                    ! local copy of edge identification for pixel
    integer :: er                                                   ! local copy of ecoregion ID
    integer,dimension(:,:),allocatable :: grid_counts               ! local array for counting number of pixels per classification in each ICM-LAVegMod grid cell - second dimension is number of landscape properties summed per cell
    real(sp),dimension(:,:),allocatable :: grid_elv_sums            ! local array for summing pixel elevations in each ICM-LAVegMod grid cell to be used for averaging - second dimension is number of landscape properties summed per cell
    integer,dimension(:),allocatable :: grid_counts_edge            ! local array for counting number of edge pixels in each ICM-LAVegMod grid cell
    integer :: grid_counts_land                                     ! local count of all pixels that are used in average elevation of land within each ICM-LAVegMod grid cell
    real(sp) :: grid_elv_sums_land                                  ! local sum of all elevation values used in calculating average elevation of land within each ICM-LAVegMod grid cell
    integer,dimension(:,:),allocatable :: comp_counts               ! local array for counting number of pixels per classification in each ICM-Hydro compartments - second dimension corresponds with landtype classification (see PARAMS for definition)
    real(sp),dimension(:,:),allocatable :: comp_elv_sums            ! local array for summing pixel elevations in each ICM-Hydro compartments to be used for averaging- second dimension corresponds with landtype classification (see PARAMS for definition)
    integer,dimension(:),allocatable :: comp_counts_edge            ! local array for counting number of edge pixels in each ICM-Hydro compartment
    integer :: comp_counts_land                                     ! local count of all pixels that are used in average elevation of land within each ICM-Hydro compartment
    real(sp) :: comp_elv_sums_land                                  ! local sum of all elevation values used in calculating average elevation of land within each ICM-Hydro compartment

    
    allocate(grid_counts(ngrid,nlt))
    allocate(grid_elv_sums(ngrid,nlt))
    allocate(grid_counts_edge(ngrid))
    allocate(comp_counts(ncomp,nlt))  
    allocate(comp_elv_sums(ncomp,nlt))
    allocate(comp_counts_edge(ncomp))

    
    grid_counts = 0
    grid_elv_sums = 0.0
    grid_counts_edge = 0
    comp_counts = 0
    comp_elv_sums = 0.0
    comp_counts_edge = 0
    er_counts = 0
    er_sum = 0
    
    
    
    write(  *,*) ' - calculating spatial averages of end-of-year landscape'
    write(000,*) ' - calculating spatial averages of end-of-year landscape'
    
    do i = 1,ndem
        g = dem_grid(i)
        c = dem_comp(i)
        lt = dem_lndtyp(i)
        z = dem_z(i)
        e = dem_edge(i)
        
        if (g /= dem_NoDataVal) then
            if (c /= dem_NoDataVal) then
                if (lt /= dem_NoDataVal) then
                    if (z /= dem_NoDataVal) then
                        grid_counts(g,lt)   = grid_counts(g,lt)   + 1
                        grid_elv_sums(g,lt) = grid_elv_sums(g,lt) + z
                        grid_counts_edge(g) = grid_counts_edge(g) + e
                        
                        comp_counts(c,lt)   = grid_counts(c,lt)   + 1
                        comp_elv_sums(c,lt) = comp_elv_sums(c,lt) + z
                        comp_counts_edge(c) = comp_counts_edge(c) + e
                        
                        er = comp_eco(c)
                        er_sum(er) = er_sum(er) + 1
                        
                        if (lt == 1) then                               !   vegetated wetland
                            er_counts(er,1) = er_counts(er,1) + 1
                            
                            if (grid_FIBS_score(g) <= 0.15 ) then       !   fresh forested wetland
                                er_counts(er,2) = er_counts(er,2) + 1      
                            else if (grid_FIBS_score(g) <= 1.5 ) then   !   fresh marsh
                                er_counts(er,3) = er_counts(er,3) + 1      
                            else if (grid_FIBS_score(g) <= 5 ) then     !   intermediate marsh
                                er_counts(er,4) = er_counts(er,4) + 1      
                            else if (grid_FIBS_score(g) <= 18 ) then    !   brackish marsh
                                er_counts(er,5) = er_counts(er,5) + 1      
                            else if (grid_FIBS_score(g) > 18 ) then     !   saline marsh
                                er_counts(er,6) = er_counts(er,6) + 1
                            end if
                        
                        else if (lt == 2) then                          !   water
                            er_counts(er,7) = er_counts(er,7) + 1
                        else if (lt == 3) then                          !   unvegetated wetland/new subaerial unvegetated mudflat (e.g., bare ground)
                            er_counts(er,8) = er_counts(er,8) + 1
                        else if (lt == 4) then                          !   developed land/upland/etc. that are not modeled in ICM-LAVegMod
                            er_counts(er,9) = er_counts(er,9) + 1
                        else if (lt == 5) then                          !   flotant marsh 
                            er_counts(er,10) = er_counts(er,10) + 1
                        end if    
                            
                        
                    end if
                end if
            end if
        end if
    end do

    do ig = 1,ngrid
        grid_pct_water(ig)  = float(grid_counts(ig,2))    / max(0.001,float(grid_ndem_all(ig)))
        grid_pct_bare(ig)   = float(grid_counts(ig,3))    / max(0.001,float(grid_ndem_all(ig)))
        grid_pct_upland(ig) = float(grid_counts(ig,4))    / max(0.001,float(grid_ndem_all(ig)))
        grid_pct_flt(ig)    = float(grid_counts(ig,5))    / max(0.001,float(grid_ndem_all(ig)))
        grid_pct_edge(ig)   = float(grid_counts_edge(ig)) / max(0.001,float(grid_ndem_all(ig)))

        ! calculate average elevation, in grid cell, of water bottom (lndtype=2)
        ! if no pixels with water are in grid cell set to NoData (-9999)
        if (grid_counts(ig,2) == 0) then
            grid_bed_z(ig) = -9999
        else
            grid_bed_z(ig) = grid_elv_sums(ig,2) / float(grid_counts(ig,2))
        end if
        
        ! calculate average elevation, in grid cell, of vegetated wetland (lndtyp=1) and nonvegetated wetland (lndtyp=3)
        ! if no pixels are vegetated nor nonvegetated wetland in grid cell set to NoData (-9999)
        grid_elv_sums_land = grid_elv_sums(ig,1 ) + grid_elv_sums(ig,3)
        grid_counts_land = grid_counts(ig,1) + grid_counts(ig,3)
        
        if (grid_counts_land == 0) then
            grid_land_z(ig) = -9999
        else
            grid_land_z(ig) = grid_elv_sums_land / float(grid_counts_land)
        end if
    end do    

    do ic=1,ncomp
        comp_pct_water(ic)  = float(comp_counts(ic,2))  / max(0.001,float(comp_ndem_all(ic)))
        comp_pct_upland(ic) = float(comp_counts(ic,4))  / max(0.001,float(comp_ndem_all(ic)))
        comp_edge_area(ic)  = comp_counts_edge(ic) * dem_res**2
        
        ! calculate average elevation, in compartment, of water bottom (lndtype=2)
        ! if no pixels with water are in compartment set to NoData (-9999)
        if (comp_counts(ic,2) == 0) then
            comp_water_z(ic) = -9999
        else
            comp_water_z(ic) = comp_elv_sums(ic,2) / float(comp_counts(ic,2))
        end if

        ! calculate average elevation, in compartment, of vegetated wetland (lndtyp=1) and nonvegetated wetland (lndtyp=3)
        ! if no pixels are vegetated nor nonvegetated wetland in compartment set to NoData (-9999)
        ! currently not including upland areas (lndtyp=4)
        comp_elv_sums_land = comp_elv_sums(ic,1 ) + comp_elv_sums(ic,3)
        comp_counts_land = comp_counts(ic,1) + comp_counts(ic,3)
        
        if (comp_counts_land == 0) then
            comp_wetland_z(ic) = -9999
        else
            comp_wetland_z(ic) = comp_elv_sums_land / float(comp_counts_land)
        end if
    end do    

    return
end