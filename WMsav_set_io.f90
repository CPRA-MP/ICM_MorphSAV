subroutine set_io
    
    !  subroutine that reads in configuration file that contains input/output files and settings
    !  refer to PARAMS for description of all variables.
    
    use params
    implicit none
    
    call params_alloc_io
    
    open(unit=001, file=trim(adjustL('geomorph/SAV_input_params.csv')))
 
    ! settings
    read(001,*) start_year,dump_txt
    read(001,*) elapsed_year,dump_txt
    read(001,*) dem_res,dump_txt                 
    read(001,*) dem_NoDataVal,dump_txt           
    read(001,*) ndem,dump_txt
    read(001,*) ndem_bi,dump_txt
    read(001,*) ncomp,dump_txt
    read(001,*) ngrid,dump_txt
    read(001,*) neco,dump_txt
    read(001,*) nlt,dump_txt
    read(001,*) ht_abv_mwl_est,dump_txt
    read(001,*) ptile_Z,dump_txt
    read(001,*) B0,dump_txt
    read(001,*) B1,dump_txt
    read(001,*) B2,dump_txt
    read(001,*) B3,dump_txt
    read(001,*) B4,dump_txt
    read(001,*) ow_bd,dump_txt
    read(001,*) om_k1,dump_txt
    read(001,*) mn_k2,dump_txt
    read(001,*) FIBS_intvals(1),dump_txt
    read(001,*) FIBS_intvals(2),dump_txt
    read(001,*) FIBS_intvals(3),dump_txt
    read(001,*) FIBS_intvals(4),dump_txt
    read(001,*) FIBS_intvals(5),dump_txt
    read(001,*) FIBS_intvals(6),dump_txt
    read(001,*) min_accretion_limit_cm,dump_txt
    read(001,*) ow_accretion_limit_cm,dump_txt
    read(001,*) ow_erosion_limit_cm,dump_txt
    read(001,*) bg_lowerZ_m,dump_txt    
    read(001,*) me_lowerDepth_m,dump_txt 
    read(001,*) flt_lowerDepth_m,dump_txt
    read(001,*) mc_depth_threshold,dump_txt
    read(001,*) spsal_params(1),dump_txt
    read(001,*) spsal_params(2),dump_txt
    read(001,*) spsal_params(3),dump_txt
    read(001,*) sptss_params(1),dump_txt
    read(001,*) sptss_params(2),dump_txt
    read(001,*) sptss_params(3),dump_txt    
    read(001,*) dfl_params(1),dump_txt
    read(001,*) dfl_params(2),dump_txt
    read(001,*) dfl_params(3),dump_txt       
    
    ! input files
    read(001,*) binary_in
    read(001,*) binary_out
    read(001,*) dem_file,dump_txt
    read(001,*) lwf_file,dump_txt
    read(001,*) meer_file,dump_txt
    read(001,*) pldr_file,dump_txt 
    read(001,*) comp_file,dump_txt
    read(001,*) grid_file,dump_txt      
    read(001,*) dsub_file,dump_txt
    read(001,*) ssub_file,dump_txt
    read(001,*) ssub_col
    read(001,*) act_del_file,dump_txt
    read(001,*) eco_omar_file,dump_txt
    read(001,*) comp_eco_file,dump_txt
    read(001,*) sav_priors_file,dump_txt
    read(001,*) hydro_comp_out_file,dump_txt
    read(001,*) prv_hydro_comp_out_file,dump_txt
    read(001,*) veg_out_file,dump_txt
    read(001,*) monthly_mean_stage_file,dump_txt
    read(001,*) monthly_max_stage_file,dump_txt
    read(001,*) monthly_ow_sed_dep_file,dump_txt
    read(001,*) monthly_mi_sed_dep_file,dump_txt
    read(001,*) monthly_me_sed_dep_file,dump_txt
    read(001,*) monthly_mean_sal_file,dump_txt
    read(001,*) monthly_mean_tss_file,dump_txt
    read(001,*) bi_dem_xyz_file,dump_txt

    !  output files     
    read(001,*) edge_eoy_xyz_file,dump_txt
    read(001,*) dem_eoy_xyz_file,dump_txt
    read(001,*) dz_eoy_xyz_file,dump_txt
    read(001,*) lndtyp_eoy_xyz_file,dump_txt
    read(001,*) lndchng_eoy_xyz_file,dump_txt
    read(001,*) salav_xyz_file
    read(001,*) salmx_xyz_file
    read(001,*) inun_xyz_file
    read(001,*) grid_summary_eoy_file,dump_txt
    read(001,*) grid_data_file,dump_txt
    read(001,*) grid_depth_file_Gdw,dump_txt
    read(001,*) grid_depth_file_GwT,dump_txt
    read(001,*) grid_depth_file_MtD,dump_txt
    read(001,*) grid_pct_edge_file,dump_txt
    read(001,*) grid_sav_file,dump_txt
    read(001,*) comp_elev_file,dump_txt
    read(001,*) comp_wat_file,dump_txt
    read(001,*) comp_upl_file,dump_txt
    
    ! QAQC save point information
    read(001,*) nqaqc                    
    read(001,*) qaqc_site_list_file
    read(001,*) fnc_tag
    
    fnc_tag =trim(adjustL(fnc_tag))
    mterm = fnc_tag(1:6)
    sterm = fnc_tag(8:10)
    gterm = fnc_tag(12:15)
    cterm = fnc_tag(17:20)
    uterm = fnc_tag(22:24)
    vterm = fnc_tag(26:28)
    

    
    close(001)

    return
end