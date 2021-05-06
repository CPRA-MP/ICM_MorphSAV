!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                  
!       ICM Wetland Morphology Submerged Aquative Vegetation (SAV) Model               
!                                                  
!                                                  
!   Fortran version of ICM-MorphSAV developed         
!   for 2023 Coastal Master Plan - LA CPRA         
!                                                  
!   original documentation: DeMarco et al., 2018       
!                                                  
!   Questions: eric.white@la.gov                   
!   last update: 11/22/2020                          
!                                                     
!   project site: https://github.com/CPRA-MP      
!   documentation: http://coastal.la.gov/our-plan  
!                                                  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
program main
    
    use params
    implicit none

    ! local variables
    integer,dimension(8) :: dtvalues                ! variable to store date time values
    
    character*17 :: dtstrf                          ! string to hold formatted datetime
    character*19 :: dtstr                           ! string to hold formatted datetime

    call date_and_time(VALUES=dtvalues)             ! grab simulation start time
    write(dtstrf,8888) dtvalues(1),dtvalues(2),dtvalues(3),'_',dtvalues(5),'.',dtvalues(6),'.',dtvalues(7)
    write(dtstr,8889) dtvalues(1),'-',dtvalues(2),'-',dtvalues(3),' ',dtvalues(5),':',dtvalues(6),':',dtvalues(7)
    open(unit=000, file=trim(adjustL('_ICM-MorphSAV_runlog_')//dtstr//trim('.log')))
    
    write(  *,*)
    write(  *,*) '*****************************************************************'
    write(  *,*) '****                                                         ****'
    write(  *,*) '****    ****    STARTING ICM-MORPH-SAV SIMULATION    ****    ****'
    write(  *,*) '****                                                         ****'
    write(  *,*) '*****************************************************************'
    write(  *,*)
    write(  *,*) 'Started ICM-MorphSAV simulation at: ',dtstr
    write(  *,*)

    write(000,*)
    write(000,*) '*****************************************************************'
    write(000,*) '****                                                         ****'
    write(000,*) '****    ****    STARTING ICM-MORPH-SAV SIMULATION    ****    ****'
    write(000,*) '****                                                         ****'
    write(000,*) '*****************************************************************'    
    write(000,*)
    write(000,*) 'Started ICM-MorphSAV simulation at: ',dtstr
    write(000,*)

    call set_io                                     ! input/output settings - must be run BEFORE parameter allocation   
    call params_alloc
   
    call preprocessing
    call date_and_time(VALUES=dtvalues)
    write(dtstr,8889) dtvalues(1),'-',dtvalues(2),'-',dtvalues(3),'_',dtvalues(5),':',dtvalues(6),':',dtvalues(7)
    write(  *,*) 'Preprocessing subroutine ended at: ',dtstr
    write(000,*) 'Preprocessing subroutine ended at: ',dtstr
 
    call distance_to_land
    call date_and_time(VALUES=dtvalues)
    write(dtstr,8889) dtvalues(1),'-',dtvalues(2),'-',dtvalues(3),' ',dtvalues(5),':',dtvalues(6),':',dtvalues(7)
    write(  *,*) 'Distance-to-land subroutine ended at: ',dtstr
    write(000,*) 'Distance-to-land subroutine ended at: ',dtstr 
    
    call sav
    call date_and_time(VALUES=dtvalues)
    write(dtstr,8889) dtvalues(1),'-',dtvalues(2),'-',dtvalues(3),' ',dtvalues(5),':',dtvalues(6),':',dtvalues(7)
    write(  *,*) 'SAV subroutine ended at: ',dtstr
    write(000,*) 'SAV subroutine ended at: ',dtstr 
    

    write(  *,*)
    write(  *,*) 'Ended ICM-MorphSAV simulation at: ',dtstr
    write(  *,*)
    
    write(000,*)
    write(000,*) 'Ended ICM-MorphSAV simulation at: ',dtstr
    write(000,*)
    close(000)


8888    format(I4.4,I2.2,I2.2,a,I2.2,a,I2.2,a,I2.2)
8889    format(I4.4,a,I2.2,a,I2.2,a,I2.2,a,I2.2,a,I2.2)
    
end program
