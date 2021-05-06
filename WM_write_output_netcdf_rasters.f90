
subroutine write_output_netcdf_rasters
    ! subroutine that writes output rasters in netcdf format
    
    use params
    use icm_netcdf,only: write_netcdf_float,write_netcdf_int
    implicit none
    
    write(  *,*) ' - writing output raster XYZ file for grid'
    write(000,*) ' - writing output raster XYZ file for grid'
    call write_netcdf_int(trim(adjustL('output/'//grid_file//'.nc')),&
                                   dem_x,dem_y,dem_grid,dble(dem_NoDataVal),"gridID")

!    write(  *,*) ' - writing output raster netCDF for topobathy DEM'
!    write(000,*) ' - writing output raster netCDF for topobathy DEM'
!    call write_netcdf_float(trim(adjustL('output/'//dem_eoy_xyz_file//'.nc')),&
!                                   dem_x,dem_y,dem_z,dble(dem_NoDataVal),"z")
!
!    write(  *,*) ' - writing output raster netCDF for landchange flag'
!    write(000,*) ' - writing output raster netCDF for landchange flag'
!    call write_netcdf_int(trim(adjustL('output/'//lndchng_eoy_xyz_file)),&
!                                   dem_x,dem_y,lnd_change_flag,&
!                                   dble(dem_NoDataVal),"landchange")
!    
!    write(  *,*) ' - writing output raster netCDF for land type'
!    write(000,*) ' - writing output raster netCDF for land type'
!    call write_netcdf_int(trim(adjustL('output/'//lndtyp_eoy_xyz_file)),&
!                                   dem_x,dem_y,dem_lndtyp,&
!                                   dble(dem_nodataval),"landtype")
!    
!    write(  *,*) ' - writing output raster netCDF for elevation change'
!    write(000,*) ' - writing output raster netCDF for elevation change'
!    call write_netcdf_float(trim(adjustL('output/'//dz_eoy_xyz_file)),&
!                                   dem_x,dem_y,dem_dz_cm,dble(dem_nodataval),&
!                                   "elevationchange")
    
    return
end subroutine write_output_netcdf_rasters
