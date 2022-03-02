subroutine preprocessing
    
    use params
    implicit none
    
    ! local variables
    integer :: i                        ! iterator
    integer :: c                        ! local compartment ID variable
    integer :: g                        ! local ICM-LAVegMod grid ID variable
    integer :: i_col                    ! X-coordinate converted to column number of mapped DEM
    integer :: i_row                    ! Y-coordinate converted to row number of mapped DEM
    integer :: dem_x_bi                 ! local variable to read in X-coord of ICM-BI-DEM interpolated point
    integer :: dem_y_bi                 ! local variable to read in Y-coord of ICM-BI-DEM interpolated point
    integer :: col_lookup               ! local variable to find DEM pixel index corresponding to ICM-BI-DEM interpolated point
    integer :: row_lookup               ! local variable to find DEM pixel index corresponding to ICM-BI-DEM interpolated point
    integer :: dem_i                    ! local variable that determined DEM pixel index corresponding to ICM-BI-DEM pixel location
    integer :: en                       ! local variable of ecoregion number used for reading in ecoregion name codes
    
    
    
    ! read pixel-to-compartment mapping file into arrays
    write(  *,*) ' - reading in DEM-pixel-to-compartment map data'
    write(000,*) ' - reading in DEM-pixel-to-compartment map data'
   
    if (binary_in == 1) then
        write(  *,*) '   - using binary file'
        write(000,*) '   - using binary file'
        open(unit=1111, file=trim(adjustL(comp_file))//'.b',form='unformatted')
        read(1111) dem_comp
    else
        open(unit=1111, file=trim(adjustL((comp_file))))
!        read(1111,*) dump_txt        ! dump header
        do i = 1,ndem
            read(1111,*) dump_int,dump_int,dem_comp(i)
        end do
    end if
    close(1111)
 
    ! read pixel-to-grid mapping file into arrays
    write(  *,*) ' - reading in DEM-pixel-to-grid cell map data'
    write(000,*) ' - reading in DEM-pixel-to-grid cell map data'
    
    if (binary_in == 1) then
        write(  *,*) '   - using binary file'
        write(000,*) '   - using binary file'
        open(unit=1112, file=trim(adjustL(grid_file))//'.b',form='unformatted')
        read(1112) dem_grid
    else
        open(unit=1112, file=trim(adjustL(grid_file)))
    !    read(1112,*) dump_txt        ! dump header
        do i = 1,ndem    
            read(1112,*) dump_int,dump_int,dem_grid(i)
        end do
    end if
    close(1112)
    
    ! map ICM-Hydro compartment ID to ICM-LAVegMod grid
    write(  *,*) ' - mapping compartment IDs to grid cell'
    write(000,*) ' - mapping compartment IDs to grid cell'
    
    grid_comp = 0                               ! initialize all grid-compartment mapping values to 0
    do i = 1,ndem
        g = dem_grid(i)
        c = dem_comp(i)
        if (g > 0) then                         ! if DEM pixel has an associated grid ID
            if (c > 0 ) then                    ! and DEM pixel has an associated compartment ID
                if (grid_comp(g) == 0) then     ! if grid-to-compartment has not yet been mapped, popuplate the grid_comp array with mapping value 
                    grid_comp(g) = c
                end if
            end if
        end if
    end do
    
!    ! read xyz file into arrays
!    write(  *,*) ' - reading in DEM data'
!    write(000,*) ' - reading in DEM data'
!    dem_x = 0
!    dem_y = 0
!    dem_z = 0.0
!    
    if (binary_in == 1) then
        write(  *,*) '   - using binary files'
        write(000,*) '   - using binary files'
        
        open(unit=11100, file=trim(adjustL('geomorph/output/raster_x_coord.b')),form='unformatted')         ! binary filename for x-coordinate is hard-set in WRITE_OUTPUT_RASTERS_BIN - it is not read in via SET_IO
        read(11100) dem_x
        close(11100) 
        
        open(unit=111000, file=trim(adjustL('geomorph/output/raster_y_coord.b')),form='unformatted')         ! binary filename for y-coordinate is hard-set in WRITE_OUTPUT_RASTERS_BIN - it is not read in via SET_IO
        read(111000) dem_y       
        close(111000)
        
        open(unit=1110, file=trim(adjustL(dem_file))//'.b',form='unformatted')
        read(1110) dem_z
    else   
        open(unit=1110, file=trim(adjustL(dem_file)))
        ! read(1110,*) dump_txt        ! dump header
        do i = 1,ndem
            read(1110,*) dem_x(i),dem_y(i),dem_z(i)
        end do
    end if

    close(1110)
    grid_ndem_all = 0                   ! before looping through all DEM pixels, initialize counter array to zero
    comp_ndem_all = 0                   ! before looping through all DEM pixels, initialize counter array to zero    
    
    ! determine lower left and upper right corners of DEM grid
    do i = 1,ndem
        if (i == 1) then
            dem_LLx = dem_x(i)
            dem_LLy = dem_y(i)
            dem_URx = dem_x(i)
            dem_URy = dem_y(i)
        else
            dem_LLx = min(dem_LLx,dem_x(i)) 
            dem_LLy = min(dem_LLy,dem_y(i))
            dem_URx = max(dem_URx,dem_y(i))
            dem_URy = max(dem_URy,dem_y(i))
        end if
        
        ! count number of DEM pixels within each ICM-Hydro compartment & ICM-LAVegMod grid cells
        if (dem_comp(i) /= dem_NoDataVal) then
            comp_ndem_all(dem_comp(i)) =  comp_ndem_all(dem_comp(i)) + 1
        end if
        if (dem_grid(i) /= dem_NoDataVal) then
            grid_ndem_all(dem_grid(i)) =  grid_ndem_all(dem_grid(i)) + 1
        end if
    end do
    
    ! set variables that hold maximum count per grid/compartment for later array allocation sizes
    grid_ndem_mx = maxval(grid_ndem_all)
    comp_ndem_mx = maxval(comp_ndem_all)
    
    ! calculate number of rows and columns in mapped DEM from X-Y ranges determined above
    n_dem_col = 1+(dem_URx - dem_LLx)/dem_res
    n_dem_row = 1+(dem_URy - dem_LLy)/dem_res

    ! allocate array for DEM map    
    call dem_params_alloc!(n_dem_col,n_dem_row)
    
    ! initialize arrays to 0
    !dem_index_mapped = 0
    
    ! loop through DEM and map pixel ID (i) to structured DEM map
    ! also save vector arrays that convert X and Y coordinate arrays into row and column arrays (searchable by DEM pixel ID, i)
    do i = 1,ndem
        i_col = 1+(dem_x(i) - dem_LLx)/dem_res
        i_row = 1+(dem_y(i) - dem_LLy)/dem_res
        dem_index_mapped(i_col,i_row) = i
    end do

   
    ! read LWF map file into arrays
    write(  *,*) ' - reading in land-water map data'
    write(000,*) ' - reading in land-water map data'
    
    if (binary_in == 1) then
        write(  *,*) '   - using binary file'
        write(000,*) '   - using binary file'
        open(unit=1113, file=trim(adjustL(lwf_file))//'.b',form='unformatted')
        read(1113) dem_lndtyp
    else
        open(unit=1113, file=trim(adjustL(lwf_file)))
        !    read(1113,*) dump_txt        ! dump header
        do i = 1,ndem 
            read(1113,*) dump_int,dump_int,dem_lndtyp(i)
        end do
    end if
    close(1113)
   
    
    ! read marsh edge erosion rate map file into arrays
    write(  *,*) ' - reading in marsh edge erosion rate map data'
    write(000,*) ' - reading in marsh edge erosion rate map data'
    if (binary_in == 1) then
        write(  *,*) '   - using binary file'
        write(000,*) '   - using binary file'
        open(unit=1114, file=trim(adjustL(meer_file))//'.b',form='unformatted')
        read(1114) dem_meer
    else    
        open(unit=1114, file=trim(adjustL(meer_file)))    
        ! read(1114,*) dump_txt        ! dump header
        do i = 1,ndem 
            read(1114,*) dump_int,dump_int,dem_meer(i)
        end do
    end if
    close(1114)
    
    
    ! read in ecoregion-to-compartment map
    write(  *,*) ' - reading in ICM-Hydro compartment-to-ecoregion lookup table'
    write(000,*) ' - reading in ICM-Hydro compartment-to-ecoregion lookup table'
    
    ! initialize grid data arrays to 0.0
    comp_eco = 0.0
    
    open(unit=1115, file=trim(adjustL(comp_eco_file)))
    read(1115,*) dump_txt                            ! dump header
    do i = 1,ncomp
        read(1115,*) dump_int,               &       ! ICM-Hydro_comp
   &                 comp_eco(i),            &       ! ecoregion number 
   &                 dump_txt,               &       ! ecoregion code
   &                 dump_txt                        ! descriptive name
    end do
    close(1115)
  
    ! read in active delta compartment flags
    write(  *,*) ' - reading in ICM-Hydro compartment table assigning active deltaic locations'
    write(000,*) ' - reading in ICM-Hydro compartment table assigning active deltaic locations'
    
    ! initialize grid data arrays to 0.0
    comp_act_dlt = 0.0
    
    open(unit=1116, file=trim(adjustL(act_del_file)))
    read(1116,*) dump_txt                               ! dump header
    do i = 1,ncomp
        read(1116,*) dump_int,comp_act_dlt(i)           ! compartment ID, active delta flag
    end do
    close(1116)
    
    ! read in deep subsidence data
    write(  *,*) ' - reading in deep susidence rate map'
    write(000,*) ' - reading in deep susidence rate map'
    
    ! initialize grid data arrays to 0.0
    dem_dpsb = 0.0
!    write(  *,*) '  ****** MISSING RASTER -  hard-coding to 1.0 mm/yr ******'
!    write(000,*) '  ****** MISSING RASTER -  hard-coding to 1.0 mm/yr ******'
!    dem_dpsb = 1.0
   
    if (binary_in == 1) then
        write(  *,*) '   - using binary file'
        write(000,*) '   - using binary file'
        open(unit=1117, file=trim(adjustL(dsub_file))//'.b',form='unformatted')
        read(1117) dem_dpsb
    else
        open(unit=1117, file=trim(adjustL(dsub_file)))
        !read(1117,*) dump_txt                               ! dump header
        do i = 1,ndem
             read(1117,*) dump_int,dump_int,dem_dpsb(i)      ! X, Y, deep subsidence
            if (dem_dpsb(i) == dem_NoDataVal) then          ! set to zero if no data
                dem_dpsb(i) = 0.0
            end if
        end do
    end if
    close(1117)
    
    ! read in shallow subsidence lookup table
    write(  *,*) ' - reading in shallow subsidence statistics by ecoregion'
    write(000,*) ' - reading in shallow subsidence statistics by ecoregion'
    
    ! initialize table to 0.0
    er_shsb = 0.0
    
    open(unit=1118, file=trim(adjustL(ssub_file)))
    read(1118,*) dump_txt                               ! dump header
    do i = 1,neco
        read(1118,*) en,                        &       ! ecoregion number
   &                er_codes(en),               &       ! ecoregion abbreviation
   &                dump_txt,                   &       ! ecoregion name
   &                er_shsb(i,1),               &       ! 25th %ile shallow subsidence rate (mm/yr) - positive is downward
   &                er_shsb(i,2),               &       ! 50th %ile shallow subsidence rate (mm/yr) - positive is downward
   &                er_shsb(i,3),               &       ! 75th %ile shallow subsidence rate (mm/yr) - positive is downward
   &                dump_txt                            ! notes
    end do
    close(1118) 
    

    ! read polder area map file into arrays
    write(  *,*) ' - reading in polder map data'
    write(000,*) ' - reading in polder map data'
    if (binary_in == 1) then
        write(  *,*) '   - using binary file'
        write(000,*) '   - using binary file'  
        open(unit=1119, file=trim(adjustL(pldr_file))//'.b',form='unformatted')
        read(1119) dem_pldr
    else    
        open(unit=1119, file=trim(adjustL(pldr_file)))    
!    read(1119,*) dump_txt        ! dump header
        do i = 1,ndem 
            read(1119,*) dump_int,dump_int,dem_pldr(i)
        end do
    end if
    close(1119)    
     
    
    ! read ICM-Hydro compartment output file into arrays
    write(  *,*) ' - reading in annual ICM-Hydro compartment-level output'
    write(000,*) ' - reading in annual ICM-Hydro compartment-level output'
    
    open(unit=112, file=trim(adjustL(hydro_comp_out_file)))
    
    read(112,*) dump_txt        ! dump header
    do i = 1,ncomp
        read(112,*) dump_txt,               &
   &         stg_mx_yr(i),                  &
   &         stg_av_yr(i),                  &
   &         stg_av_smr(i),                 &
   &         stg_sd_smr(i),                 &
   &         sal_av_yr(i),                  &
   &         sal_av_smr(i),                 &
   &         sal_mx_14d_yr(i),              &
   &         tmp_av_yr(i),                  &
   &         tmp_av_smr(i),                 &
   &         sed_dp_ow_yr(i),               &
   &         sed_dp_mi_yr(i),               &
   &         sed_dp_me_yr(i),               &
   &         tidal_prism_ave(i),            &
   &         ave_sepmar_stage(i),           &
   &         ave_octapr_stage(i),           &
   &         marsh_edge_erosion_rate(i),    &
   &         ave_annual_tss(i),             &
   &         stdev_annual_tss(i),           &
   &         totalland_m2(i)
    end do
    close(112)
    
    ! read previous year's ICM-Hydro compartment output file into arrays
    write(  *,*) ' - reading in previous year annual ICM-Hydro compartment-level output'
    write(000,*) ' - reading in previous year annual ICM-Hydro compartment-level output'
    
    open(unit=1120, file=trim(adjustL(prv_hydro_comp_out_file)))
    
    read(1120,*) dump_txt        ! dump header
    do i = 1,ncomp
        read(1120,*) dump_txt,               &
   &         dump_flt    ,                  &
   &         stg_av_prev_yr(i),             &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         sal_av_prev_yr(i),             &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt    ,                  &
   &         dump_flt
    end do
    close(1120)

    ! read in monthly data for stage and sediment deposition (open water and marsh)    
    write(  *,*) ' - reading in monthly ICM-Hydro compartment-level output'
    write(000,*) ' - reading in monthly ICM-Hydro compartment-level output'

    open(unit=113, file=trim(adjustL(monthly_mean_stage_file)))
    open(unit=114, file=trim(adjustL(monthly_max_stage_file)))
    open(unit=115, file=trim(adjustL(monthly_ow_sed_dep_file)))
    open(unit=116, file=trim(adjustL(monthly_mi_sed_dep_file)))
    open(unit=117, file=trim(adjustL(monthly_me_sed_dep_file)))
    open(unit=118, file=trim(adjustL(monthly_mean_sal_file)))
    open(unit=119, file=trim(adjustL(monthly_mean_tss_file)))

    
    read(113,*) dump_txt        ! dump header
    read(114,*) dump_txt        ! dump header
    read(115,*) dump_txt        ! dump header
    read(116,*) dump_txt        ! dump header
    read(117,*) dump_txt        ! dump header
    read(118,*) dump_txt        ! dump header
    read(119,*) dump_txt        ! dump header

    
    do i = 1,ncomp
        read(113,*) dump_int,                   &
   &         stg_av_mons(i,1),                  &
   &         stg_av_mons(i,2),                  &
   &         stg_av_mons(i,3),                  &
   &         stg_av_mons(i,4),                  &
   &         stg_av_mons(i,5),                  &
   &         stg_av_mons(i,6),                  &
   &         stg_av_mons(i,7),                  &
   &         stg_av_mons(i,8),                  &
   &         stg_av_mons(i,9),                  &
   &         stg_av_mons(i,10),                 &
   &         stg_av_mons(i,11),                 &
   &         stg_av_mons(i,12)
        
        read(114,*) dump_int,                   &
   &         stg_mx_mons(i,1),                  &
   &         stg_mx_mons(i,2),                  &
   &         stg_mx_mons(i,3),                  &
   &         stg_mx_mons(i,4),                  &
   &         stg_mx_mons(i,5),                  &
   &         stg_mx_mons(i,6),                  &
   &         stg_mx_mons(i,7),                  &
   &         stg_mx_mons(i,8),                  &
   &         stg_mx_mons(i,9),                  &
   &         stg_mx_mons(i,10),                 &
   &         stg_mx_mons(i,11),                 &
   &         stg_mx_mons(i,12)       
        
        read(115,*) dump_int,                   &
   &         sed_dp_ow_mons(i,1),               &
   &         sed_dp_ow_mons(i,2),               &
   &         sed_dp_ow_mons(i,3),               &
   &         sed_dp_ow_mons(i,4),               &
   &         sed_dp_ow_mons(i,5),               &
   &         sed_dp_ow_mons(i,6),               &
   &         sed_dp_ow_mons(i,7),               &
   &         sed_dp_ow_mons(i,8),               &
   &         sed_dp_ow_mons(i,9),               &
   &         sed_dp_ow_mons(i,10),              &
   &         sed_dp_ow_mons(i,11),              &
   &         sed_dp_ow_mons(i,12)        
        
        read(116,*) dump_int,                   &
   &         sed_dp_mi_mons(i,1),               &
   &         sed_dp_mi_mons(i,2),               &
   &         sed_dp_mi_mons(i,3),               &
   &         sed_dp_mi_mons(i,4),               &
   &         sed_dp_mi_mons(i,5),               &
   &         sed_dp_mi_mons(i,6),               &
   &         sed_dp_mi_mons(i,7),               &
   &         sed_dp_mi_mons(i,8),               &
   &         sed_dp_mi_mons(i,9),               &
   &         sed_dp_mi_mons(i,10),              &
   &         sed_dp_mi_mons(i,11),              &
   &         sed_dp_mi_mons(i,12)
        
        read(117,*) dump_int,                   &
   &         sed_dp_me_mons(i,1),               &
   &         sed_dp_me_mons(i,2),               &
   &         sed_dp_me_mons(i,3),               &
   &         sed_dp_me_mons(i,4),               &
   &         sed_dp_me_mons(i,5),               &
   &         sed_dp_me_mons(i,6),               &
   &         sed_dp_me_mons(i,7),               &
   &         sed_dp_me_mons(i,8),               &
   &         sed_dp_me_mons(i,9),               &
   &         sed_dp_me_mons(i,10),              &
   &         sed_dp_me_mons(i,11),              &
   &         sed_dp_me_mons(i,12)        
   
           read(118,*) dump_int,                   &
   &         sal_av_mons(i,1),               &
   &         sal_av_mons(i,2),               &
   &         sal_av_mons(i,3),               &
   &         sal_av_mons(i,4),               &
   &         sal_av_mons(i,5),               &
   &         sal_av_mons(i,6),               &
   &         sal_av_mons(i,7),               &
   &         sal_av_mons(i,8),               &
   &         sal_av_mons(i,9),               &
   &         sal_av_mons(i,10),              &
   &         sal_av_mons(i,11),              &
   &         sal_av_mons(i,12)        
   
           read(119,*) dump_int,                   &
   &         tss_av_mons(i,1),               &
   &         tss_av_mons(i,2),               &
   &         tss_av_mons(i,3),               &
   &         tss_av_mons(i,4),               &
   &         tss_av_mons(i,5),               &
   &         tss_av_mons(i,6),               &
   &         tss_av_mons(i,7),               &
   &         tss_av_mons(i,8),               &
   &         tss_av_mons(i,9),               &
   &         tss_av_mons(i,10),              &
   &         tss_av_mons(i,11),              &
   &         tss_av_mons(i,12)        

    end do
    
    close(113)
    close(114)
    close(115)
    close(116)            
    close(117)
    close(118)
    close(119)
    
    
    
     ! read ICM-LAVegMod grid output file into arrays
    write(  *,*) ' - reading in ICM-LAVegMod grid-level output'
    write(000,*) ' - reading in ICM-LAVegMod grid-level output'
    
    ! initialize grid data arrays
    grid_pct_water = 0.0
    grid_pct_upland = 0.0
    grid_pct_bare_old = 0.0
    grid_pct_bare_new = 0.0
    grid_pct_dead_flt = 0.0
    grid_bed_z = 0.0
    grid_land_z = 0.0
    grid_FIBS_score = dem_NoDataVal
    
    open(unit=120, file=trim(adjustL(veg_out_file)))

    do i = 1,6
        read(120,*) dump_txt        ! dump ASCI grid header rows    
    end do

    do i = 1,365
        read(120,*) dump_txt        ! dump ASCI grid
    end do

    read(120,1234) dump_txt         ! dump column header row ! format 1234 must match structure of veg_out_file column headers

    do i = 1,ngrid
                read(120,*) g,                                      &      ! CELLID
   &                grid_pct_water(g),                              &      ! WATER
   &                grid_pct_upland(g),                             &      ! NOTMOD
   &                grid_pct_bare_old(g),                           &      ! BAREGRND_OLD
   &                grid_pct_bare_new(g),                           &      ! BAREGRND_NEW
   &                dump_flt,                                       &      ! QULA3
   &                dump_flt,                                       &      ! QULE
   &                dump_flt,                                       &      ! QUNI
   &                dump_flt,                                       &      ! QUTE
   &                dump_flt,                                       &      ! QUVI
   &                dump_flt,                                       &      ! ULAM
   &                dump_flt,                                       &      ! NYAQ2
   &                dump_flt,                                       &      ! SANI
   &                dump_flt,                                       &      ! TADI2
   &                dump_flt,                                       &      ! ELBA2_Flt
   &                dump_flt,                                       &      ! PAHE2_Flt
   &                dump_flt,                                       &      ! BAREGRND_Flt
   &                grid_pct_dead_flt(g),                           &      ! DEAD_Flt
   &                dump_flt,                                       &      ! COES
   &                dump_flt,                                       &      ! MOCE2
   &                dump_flt,                                       &      ! PAHE2
   &                dump_flt,                                       &      ! SALA2
   &                dump_flt,                                       &      ! ZIMI
   &                dump_flt,                                       &      ! CLMA10
   &                dump_flt,                                       &      ! ELCE
   &                dump_flt,                                       &      ! IVFR
   &                dump_flt,                                       &      ! PAVA
   &                dump_flt,                                       &      ! PHAU7
   &                dump_flt,                                       &      ! POPU5
   &                dump_flt,                                       &      ! SALA
   &                dump_flt,                                       &      ! SCCA11
   &                dump_flt,                                       &      ! TYDO
   &                dump_flt,                                       &      ! SCAM6
   &                dump_flt,                                       &      ! SCRO5
   &                dump_flt,                                       &      ! SPCY
   &                dump_flt,                                       &      ! SPPA
   &                dump_flt,                                       &      ! AVGE
   &                dump_flt,                                       &      ! DISP
   &                dump_flt,                                       &      ! JURO
   &                dump_flt,                                       &      ! SPAL
   &                dump_flt,                                       &      ! BAHABI
   &                dump_flt,                                       &      ! DISPBI
   &                dump_flt,                                       &      ! PAAM2
   &                dump_flt,                                       &      ! SOSE
   &                dump_flt,                                       &      ! SPPABI
   &                dump_flt,                                       &      ! SPVI3
   &                dump_flt,                                       &      ! STHE9
   &                dump_flt,                                       &      ! UNPA
   &                grid_FIBS_score(g),                             &      ! FFIBS
   &                grid_pct_vglnd_BLHF(g),                         &      ! pL_BF
   &                grid_pct_vglnd_SWF(g),                          &      ! pL_SF
   &                grid_pct_vglnd_FM(g),                            &      ! pL_FM
   &                grid_pct_vglnd_IM(g),                           &      ! pL_IM
   &                grid_pct_vglnd_BM(g),                           &      ! pL_BM
   &                grid_pct_vglnd_SM(g)                                   ! pL_SM

    end do
    close(120)
    
    
    
    ! read ICM-LAVegMod grid output file into arrays
    write(  *,*) ' - reading in ecoregion orgranic accumulation tables'
    write(000,*) ' - reading in ecoregion orgranic accumulation tables'
    
    er_omar = 0.0
    
    open(unit=121, file=trim(adjustL(eco_omar_file)))
    read(121,*) dump_txt        ! dump header
    
    do i=1,neco                               
        read(121,*) dump_int,                                       &      ! er_n,
   &                dump_txt,                                       &      ! er,
   &                dump_flt,                                       &      ! SwampOrgAccum_g_cm^-2_yr^-1_lower,
   &                er_omar(i,1),                                   &      ! SwampOrgAccum_g_cm^-2_yr^-1_median,
   &                dump_flt,                                       &      ! SwampOrgAccum_g_cm^-2_yr^-1_upper,
   &                dump_flt,                                       &      ! FreshOrgAccum_g_cm^-2_yr^-1_lower,
   &                er_omar(i,2),                                   &      ! FreshOrgAccum_g_cm^-2_yr^-1_median,
   &                dump_flt,                                       &      ! FreshOrgAccum_g_cm^-2_yr^-1_upper,
   &                dump_flt,                                       &      ! InterOrgAccum_g_cm^-2_yr^-1_lower,
   &                er_omar(i,3),                                   &      ! InterOrgAccum_g_cm^-2_yr^-1_median,
   &                dump_flt,                                       &      ! InterOrgAccum_g_cm^-2_yr^-1_upper,
   &                dump_flt,                                       &      ! BrackOrgAccum_g_cm^-2_yr^-1_lower,
   &                er_omar(i,4),                                   &      ! BrackOrgAccum_g_cm^-2_yr^-1_median,
   &                dump_flt,                                       &      ! BrackOrgAccum_g_cm^-2_yr^-1_upper,
   &                dump_flt,                                       &      ! SalineOrgAccum_g_cm^-2_yr^-1_lower,
   &                er_omar(i,5),                                   &      ! SalineOrgAccum_g_cm^-2_yr^-1_median,
   &                dump_flt,                                       &      ! SalineOrgAccum_g_cm^-2_yr^-1_upper,
   &                dump_flt,                                       &      ! ActiveFreshOrgAccum_g_cm^-2_yr^-1_lower,
   &                er_omar(i,6),                                   &      ! ActiveFreshOrgAccum_g_cm^-2_yr^-1_median,
   &                dump_flt                                               ! ActiveFreshOrgAccum_g_cm^-2_yr^-1_upper
    end do                                                                 
    close(121)                                                             
                                                                           
    ! read ICM-BI-DEM area map file into arrays
    write(  *,*) ' - reading in ICM-BI-DEM and mapping lookup to main DEM'
    write(000,*) ' - reading in ICM-BI-DEM and mapping lookup to main DEM'

    dem_to_bidem = dem_NoDataVal                                            ! initialize DEM-to-ICM-BI-DEM map lookup array to NoData

    open(unit=122, file=trim(adjustL(bi_dem_xyz_file)))    
!    read(120,*) dump_txt        ! dump header
    do i = 1,ndem_bi
        read(122,*) dem_x_bi, dem_y_bi, dem_z_bi(i)
   
        col_lookup = 1+(dem_x_bi - dem_LLx)/dem_res                         ! find column number in mapped DEM that corresponds to BI-DEM X-coord
        row_lookup = 1+(dem_y_bi - dem_LLy)/dem_res                         ! find row number in mapped DEM that corresponds to BI-DEM Y-coord
        dem_i = dem_index_mapped(col_lookup,row_lookup)                     ! find index number of DEM pixel that corresponds to BI-DEM XY-coordinates
        dem_to_bidem(dem_i) = i
    
    end do    
    close(122)    

    ! read in SAV statistical model priors file
    write(  *,*) ' - reading in parameters defining SAV statistical model priors by ecoregion'
    write(000,*) ' - reading in parameters defining SAV statistical model priors by ecoregion'
    
    ! initialize table to noData
    prior_int = dem_NoDataVal
    prior_slope = dem_NoDataVal

    open(unit=123, file=trim(adjustL(sav_priors_file)))
    read(123,*) dump_txt                               ! dump header
    do i = 1,neco
        read(123,*) en,                             &       ! ecoregion number
   &                dump_txt,                       &       ! ecoregion abbreviation
   &                dump_txt,                       &       ! ecoregion name
   &                dump_txt,                       &       ! CWPPRA basin name
   &                dump_int,                       &       ! CWPPRA basin number
   &                prior_int(en),                  &       ! intercept value for SAV statstical prior model for basin
   &                prior_slope(en)                         ! slope value for SAV statstical prior model for basin
    end do
    close(123) 
    
    
    
1234    format(A,53(',',A))
    
    return
end
