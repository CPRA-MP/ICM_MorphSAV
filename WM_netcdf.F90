module icm_netcdf
#ifdef ICM_NETCDF
    use netcdf
#endif    
    implicit none

    contains
#ifdef ICM_NETCDF
    !*************************************
    ! netcdf status checking routine
    !
    ! code exits if there is a non-zero status
    ! returned from the netcdf library
    !************************************
    subroutine nccheck(nc_status)
    !************************************
        implicit none
        integer,intent(in) :: nc_status
        character(200)     :: err_str
        if(nc_status.ne.nf90_noerr)then
            write(*,'(2A)') "Error from netCDF: ",NF90_STRERROR(nc_status)
            call exit(1)
        endif
    !************************************
    end subroutine
    !************************************

    !************************************
    subroutine write_netcdf_header(fn,x,y,vartype,fill,varname,fid,varid_z)
    !************************************
        implicit none
        character(*),intent(in) :: fn
        character(*),intent(in) :: varname
        integer,intent(in)      :: x(:)
        integer,intent(in)      :: y(:)
        integer,intent(in)      :: vartype
        real(8),intent(in)      :: fill
        integer,intent(out)     :: fid
        integer,intent(out)     :: varid_z

        integer,parameter       :: use_compression = 1

        integer                 :: nx
        integer                 :: ny
        real(8),allocatable     :: cell_x(:)
        real(8),allocatable     :: cell_y(:)
        integer                 :: dimid_nx
        integer                 :: dimid_ny
        integer                 :: varid_x
        integer                 :: varid_y

        nx = size(x)
        ny = size(y)

        cell_x = dble(x)
        cell_y = dble(y)

        !...Open the netCDF file in hdf5+classic mode
        call nccheck(nf90_create(fn,ior(nf90_hdf5,nf90_classic_model),fid))
    
        !...define the dimensions for x, y, z
        call nccheck(nf90_def_dim(fid,'x',nx,dimid_nx))
        call nccheck(nf90_def_dim(fid,'y',ny,dimid_ny))
    
        !...define the variables
        call nccheck(nf90_def_var(fid,'x',nf90_double,dimid_nx,varid_x))
        call nccheck(nf90_def_var(fid,'y',nf90_double,dimid_ny,varid_y))

        if(vartype.eq.nf90_int)then
            call nccheck(nf90_def_var(fid,'z',nf90_double,(/dimid_nx,dimid_ny/),varid_z))
            call nccheck(nf90_def_var_fill(fid,varid_z,0,int(fill)))
        elseif(vartype.eq.nf90_float)then
            call nccheck(nf90_def_var(fid,'z',nf90_float,(/dimid_nx,dimid_ny/),varid_z))
            call nccheck(nf90_def_var_fill(fid,varid_z,0,fill))
        elseif(vartype.eq.nf90_double)then
            call nccheck(nf90_def_var(fid,'z',nf90_double,(/dimid_nx,dimid_ny/),varid_z))
            call nccheck(nf90_def_var_fill(fid,varid_z,0,fill))
        else
            write(*,'(A)') "ERROR: invalid variable type"
            call exit(1)
        endif

        !...set the netcdf compression options (if turned on)
        if(use_compression.eq.1)then
            call nccheck(nf90_def_var_deflate(fid,varid_x,1,1,2))
            call nccheck(nf90_def_var_deflate(fid,varid_y,1,1,2))
            call nccheck(nf90_def_var_deflate(fid,varid_z,1,1,2))
        endif

        !...metadata definitions
        !  [ Add metadata defintions here as: ]
        ! Option 1: Metadata for variable:
        !  call nccheck(nf90_put_att(fid,varid,"attribute",attribute_value))
        ! Option 2: Global metadata
        !  call nccheck(nf90_put_att(fid,nf90_global,"attribute",attribute_value))
    
        !...end of definitions
        call nccheck(nf90_enddef(fid))

        call nccheck(nf90_put_var(fid,varid_x,x))
        call nccheck(nf90_put_var(fid,varid_y,y))
    !************************************
    end subroutine write_netcdf_header
    !************************************
#endif

    
    !************************************
    subroutine write_netcdf_float(fn, x, y, z, fillval, varname)
    !************************************
        use params,only:sp,dp
        implicit none
        character(*),intent(in) :: fn
        character(*),intent(in) :: varname
        integer,intent(in)      :: x(:)
        integer,intent(in)      :: y(:)
        real(sp),intent(in)     :: z(:)
        real(8),intent(in)      :: fillval
        integer                 :: fid
        integer                 :: varid_z
    
#ifndef ICM_NETCDF        
        write(*,'(A)') "ERROR: ICM-Morph not compiled for netCDF"
        call exit(1)
#else        
        !...Initialze the output with type float
        call write_netcdf_header(fn,x,y,nf90_float,fillval,varname,fid,varid_z)

        !...write the variables to the file
        call nccheck(nf90_put_var(fid,varid_z,z))
        
        !...Close the netcdf file
        call nccheck(nf90_close(fid))
#endif
    !************************************
    end subroutine write_netcdf_float
    !************************************
    
    !************************************
    subroutine write_netcdf_double(fn, x, y, z, fillval, varname)
    !************************************
        use params,only:dp
        implicit none
        character(*),intent(in) :: fn
        character(*),intent(in) :: varname
        integer,intent(in)      :: x(:)
        integer,intent(in)      :: y(:)
        real(dp),intent(in)     :: z(:)
        real(8),intent(in)      :: fillval
        integer                 :: fid
        integer                 :: varid_z
    
#ifndef ICM_NETCDF        
        write(*,'(A)') "ERROR: ICM-Morph not compiled for netCDF"
        call exit(1)
#else        
        !...Initialze the output with type double
        call write_netcdf_header(fn,x,y,nf90_double,fillval,varname,fid,varid_z)

        !...write the variables to the file
        call nccheck(nf90_put_var(fid,varid_z,z))
        
        !...Close the netcdf file
        call nccheck(nf90_close(fid))
#endif    
    !************************************
    end subroutine write_netcdf_double
    !************************************
    
    !************************************
    subroutine write_netcdf_int(fn, x, y, z, fillval, varname)
    !************************************
        implicit none
        character(*),intent(in) :: fn
        character(*),intent(in) :: varname
        integer,intent(in)      :: x(:)
        integer,intent(in)      :: y(:)
        integer,intent(in)      :: z(:)
        real(8),intent(in)      :: fillval
        integer                 :: fid
        integer                 :: varid_z
    
#ifndef ICM_NETCDF        
        write(*,'(A)') "ERROR: ICM-Morph not compiled for netCDF"
        call exit(1)
#else        
        !...Initialze the output with type integer
        call write_netcdf_header(fn,x,y,nf90_int,fillval,varname,fid,varid_z)

        !...write the variables to the file
        call nccheck(nf90_put_var(fid,varid_z,z))
        
        !...Close the netcdf file
        call nccheck(nf90_close(fid))
#endif
    !************************************
    end subroutine write_netcdf_int
    !************************************
end module icm_netcdf
