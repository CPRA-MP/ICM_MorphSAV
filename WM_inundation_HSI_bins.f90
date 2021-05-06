subroutine inundation_HSI_bins
    ! subroutine that processes specific output data needed only in ICM-HSI
    !
    ! global arrays updated by subroutine:
    !      grid_gadwl_dep
    !      grid_gwteal_dep 
    !      grid_motduck_dep   

    use params
    implicit none
    
    ! local variables
    integer :: i                                ! iterator
    integer :: m                                ! local variable for month to use for mean depth calculation from dem_inun_dep array
    integer :: g                                ! local variable for ICM-LAVegMod grid cell number
    real(sp) :: dep_ann                         ! local variable for mean annual depth at DEM pixel
    real(sp) :: dep_oct_apr                     ! local variable for mean October through April depth at DEM pixel
    real(sp) :: dep_sep_mar                     ! local variable for mean September through March depth at DEM pixel

    ! initialize depth area arrays to zero
    grid_gadwl_dep = 0
    grid_gwteal_dep = 0
    grid_motduck_dep = 0
    
    write(  *,'(A)') '  - tabulating area within each depth bin for waterfowl HSIs'
    write(000,'(A)') '  - tabulating area within each depth bin for waterfowl HSIs'
    
    do i=1,ndem
        g = dem_grid(i)
 
        ! calculate mean depth values for different month ranges
        ! Greenwing Teal mean depth:    September-March
        ! Gadwall mean depth:           October-April
        ! Mottled Duck mean depth :     annual

        dep_sep_mar = 0.0
        dep_oct_apr = 0.0
        dep_ann = 0.0
        
        do m = 1,3
            dep_sep_mar = dep_sep_mar + dem_inun_dep(i,m)
            dep_oct_apr = dep_oct_apr + dem_inun_dep(i,m)
        end do

        dep_oct_apr = dep_oct_apr + dem_inun_dep(i,4)
        dep_sep_mar = dep_sep_mar + dem_inun_dep(i,9)

        do m = 10,12
            dep_sep_mar = dep_sep_mar + dem_inun_dep(i,m)
            dep_oct_apr = dep_oct_apr + dem_inun_dep(i,m)
        end do
        
        dep_sep_mar = dep_sep_mar/7.0
        dep_oct_apr = dep_oct_apr/7.0
        
        dep_ann = dem_inun_dep(i,13)
        
        ! tabulate area of grid cell within each Gadwall depth bin
        ! depth thresholds (in m) are: [0,0.04,0.08,0.12,0.18,0.22,0.28,0.32,0.36,0.40,0.44,0.78,1.50]
        if (dep_oct_apr <= 0.0) then
            grid_gadwl_dep(g,1)  = grid_gadwl_dep(g,1) + dem_res**2
        else if (dep_oct_apr <= 0.04) then
            grid_gadwl_dep(g,2)  = grid_gadwl_dep(g,2) + dem_res**2
        else if (dep_oct_apr <= 0.08) then
            grid_gadwl_dep(g,3)  = grid_gadwl_dep(g,3) + dem_res**2
        else if (dep_oct_apr <= 0.12) then
            grid_gadwl_dep(g,4)  = grid_gadwl_dep(g,4) + dem_res**2
        else if (dep_oct_apr <= 0.18) then
            grid_gadwl_dep(g,5)  = grid_gadwl_dep(g,5) + dem_res**2
        else if (dep_oct_apr <= 0.22) then
            grid_gadwl_dep(g,6)  = grid_gadwl_dep(g,6) + dem_res**2
        else if (dep_oct_apr <= 0.28) then
            grid_gadwl_dep(g,7)  = grid_gadwl_dep(g,7) + dem_res**2
        else if (dep_oct_apr <= 0.32) then
            grid_gadwl_dep(g,8)  = grid_gadwl_dep(g,8) + dem_res**2
        else if (dep_oct_apr <= 0.36) then
            grid_gadwl_dep(g,9)  = grid_gadwl_dep(g,9) + dem_res**2
        else if (dep_oct_apr <= 0.40) then
            grid_gadwl_dep(g,10) = grid_gadwl_dep(g,10) + dem_res**2
        else if (dep_oct_apr <= 0.44) then
            grid_gadwl_dep(g,11) = grid_gadwl_dep(g,11) + dem_res**2
        else if (dep_oct_apr <= 0.78) then
            grid_gadwl_dep(g,12) = grid_gadwl_dep(g,12) + dem_res**2
        else if (dep_oct_apr <= 1.50) then
            grid_gadwl_dep(g,13) = grid_gadwl_dep(g,13) + dem_res**2
        else
            grid_gadwl_dep(g,14) = grid_gadwl_dep(g,14) + dem_res**2
        end if
        

        ! tabulate area of grid cell within each Greenwing Teal depth bin
        ! depth thresholds (in m) are: [0,0.06,0.18,0.22,0.26,0.30,0.34,1.0]
        if (dep_sep_mar <= 0.0) then
            grid_gwteal_dep(g,1) = grid_gwteal_dep(g,1) + dem_res**2
        else if (dep_sep_mar <= 0.06) then
            grid_gwteal_dep(g,2) = grid_gwteal_dep(g,2) + dem_res**2
        else if (dep_sep_mar <= 0.18) then
            grid_gwteal_dep(g,3) = grid_gwteal_dep(g,3) + dem_res**2
        else if (dep_sep_mar <= 0.22) then
            grid_gwteal_dep(g,4) = grid_gwteal_dep(g,4) + dem_res**2
        else if (dep_sep_mar <= 0.26) then
            grid_gwteal_dep(g,5) = grid_gwteal_dep(g,5) + dem_res**2
        else if (dep_sep_mar <= 0.30) then
            grid_gwteal_dep(g,6) = grid_gwteal_dep(g,6) + dem_res**2
        else if (dep_sep_mar <= 0.34) then
            grid_gwteal_dep(g,7) = grid_gwteal_dep(g,7) + dem_res**2
        else if (dep_sep_mar <= 1.0) then
            grid_gwteal_dep(g,8) = grid_gwteal_dep(g,8) + dem_res**2
        else
            grid_gwteal_dep(g,9) = grid_gwteal_dep(g,9) + dem_res**2
        end if        
           
        ! tabulate area of grid cell within each Mottled Duck depth bin
        ! depth thresholds (in m) are: [0,0.08,0.30,0.36,0.42,0.46,0.50,0.56]
        if (dep_ann <= 0.0) then        
            grid_motduck_dep(g,1) = grid_motduck_dep(g,1) + dem_res**2
        else if (dep_ann <= 0.08) then            
            grid_motduck_dep(g,2) = grid_motduck_dep(g,2) + dem_res**2
        else if (dep_ann <= 0.30) then            
            grid_motduck_dep(g,3) = grid_motduck_dep(g,3) + dem_res**2
        else if (dep_ann <= 0.36) then            
            grid_motduck_dep(g,4) = grid_motduck_dep(g,4) + dem_res**2
        else if (dep_ann <= 0.42) then            
            grid_motduck_dep(g,5) = grid_motduck_dep(g,5) + dem_res**2
        else if (dep_ann <= 0.46) then            
            grid_motduck_dep(g,6) = grid_motduck_dep(g,6) + dem_res**2
        else if (dep_ann <= 0.50) then            
            grid_motduck_dep(g,7) = grid_motduck_dep(g,7) + dem_res**2
        else if (dep_ann <= 0.56) then            
            grid_motduck_dep(g,8) = grid_motduck_dep(g,8) + dem_res**2
        else
            grid_motduck_dep(g,9) = grid_motduck_dep(g,9) + dem_res**2
        endif
        
    end do
        
    return

end