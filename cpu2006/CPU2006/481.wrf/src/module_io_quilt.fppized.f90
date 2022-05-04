!WRF:DRIVER_LAYER:IO
!
!#define mpi_x_comm_size(i,j,k)  Mpi_Comm_Size ( i,j,k ) ; write(0,*) 4

MODULE module_ext_quilt
  INTEGER, PARAMETER :: int_num_handles = 99
  LOGICAL, DIMENSION(int_num_handles) :: okay_to_write, int_handle_in_use, okay_to_commit
  INTEGER, DIMENSION(int_num_handles) :: int_num_bytes_to_write, io_form
  REAL, POINTER    :: int_local_output_buffer(:)
  INTEGER          :: int_local_output_cursor
  LOGICAL          :: quilting_enabled
  LOGICAL          :: disable_quilt = .FALSE.


  CONTAINS


    !--- ioinit
    SUBROUTINE init_module_ext_quilt
      RETURN
    END SUBROUTINE init_module_ext_quilt
END MODULE module_ext_quilt

! Call this in programs that you never want to be quilting (e.g. real)
! Must call before call to init_module_ext_quilt
!
SUBROUTINE disable_quilting
  USE module_ext_quilt
  disable_quilt = .TRUE.
  RETURN
END SUBROUTINE disable_quilting

LOGICAL FUNCTION  use_output_servers()
  USE module_ext_quilt
  use_output_servers = quilting_enabled
  RETURN
END FUNCTION use_output_servers

LOGICAL FUNCTION  use_input_servers()
  USE module_ext_quilt
  use_input_servers = .FALSE.
  RETURN
END FUNCTION use_input_servers

!--- open_for_write_begin
SUBROUTINE ext_quilt_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , io_form_arg, Status )
  RETURN  
END SUBROUTINE ext_quilt_open_for_write_begin

!--- open_for_write_commit
SUBROUTINE ext_quilt_open_for_write_commit( DataHandle , Status )
  RETURN  
END SUBROUTINE ext_quilt_open_for_write_commit

!--- open_for_read 
SUBROUTINE ext_quilt_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )
  RETURN  
END SUBROUTINE ext_quilt_open_for_read

!--- intio_nextrec  (INT_IO only)
SUBROUTINE ext_quilt_intio_nextrec ( DataHandle , NextRec , Status )
  RETURN  
END SUBROUTINE ext_quilt_intio_nextrec

!--- inquire_opened
SUBROUTINE ext_quilt_inquire_opened ( DataHandle, FileName , FileStatus, Status )
  RETURN
END SUBROUTINE ext_quilt_inquire_opened

!--- inquire_filename
SUBROUTINE ext_quilt_inquire_filename ( DataHandle, FileName , FileStatus, Status )
  RETURN
END SUBROUTINE ext_quilt_inquire_filename

!--- sync
SUBROUTINE ext_quilt_iosync ( DataHandle, Status )
  RETURN
END SUBROUTINE ext_quilt_iosync

!--- close
SUBROUTINE ext_quilt_ioclose ( DataHandle, Status )
  RETURN
END SUBROUTINE ext_quilt_ioclose

!--- ioexit
SUBROUTINE ext_quilt_ioexit( Status )
  RETURN  
END SUBROUTINE

SUBROUTINE server_io_exit( Status )
  RETURN  
END SUBROUTINE

!--- get_next_time (not defined for IntIO )
SUBROUTINE ext_quilt_get_next_time ( DataHandle, DateStr, Status )
  RETURN
END SUBROUTINE ext_quilt_get_next_time

!--- get_previous_time (not defined for IntIO )
SUBROUTINE ext_quilt_get_previous_time ( DataHandle, DateStr, Status )
  RETURN
END SUBROUTINE ext_quilt_get_previous_time

!--- put_dom_ti_char
SUBROUTINE ext_quilt_set_time ( DataHandle, Data,  Status )
RETURN
END SUBROUTINE ext_quilt_set_time

!--- get_next_var  (not defined for IntIO)
SUBROUTINE ext_quilt_get_next_var ( DataHandle, VarName, Status )
  RETURN
END SUBROUTINE ext_quilt_get_next_var

!--- get_dom_ti_real
SUBROUTINE ext_quilt_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_real 

!--- put_dom_ti_real
SUBROUTINE ext_quilt_put_dom_ti_real ( DataHandle,Element,   Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_real 

!--- get_dom_ti_double
SUBROUTINE ext_quilt_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_double 

!--- put_dom_ti_double
SUBROUTINE ext_quilt_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_double 

!--- get_dom_ti_integer
SUBROUTINE ext_quilt_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_integer 

!--- put_dom_ti_integer
SUBROUTINE ext_quilt_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_integer 

!--- get_dom_ti_logical
SUBROUTINE ext_quilt_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_logical 

!--- put_dom_ti_logical
SUBROUTINE ext_quilt_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_logical 

!--- get_dom_ti_char
SUBROUTINE ext_quilt_get_dom_ti_char ( DataHandle,Element,   Data,  Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_char 

!--- put_dom_ti_char
SUBROUTINE ext_quilt_put_dom_ti_char ( DataHandle, Element,  Data,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_char 

!--- get_dom_td_real
SUBROUTINE ext_quilt_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_td_real 

!--- put_dom_td_real
SUBROUTINE ext_quilt_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_td_real 

!--- get_dom_td_double
SUBROUTINE ext_quilt_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_td_double 

!--- put_dom_td_double
SUBROUTINE ext_quilt_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_td_double 

!--- get_dom_td_integer
SUBROUTINE ext_quilt_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_td_integer 

!--- put_dom_td_integer
SUBROUTINE ext_quilt_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_td_integer 

!--- get_dom_td_logical
SUBROUTINE ext_quilt_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_td_logical 

!--- put_dom_td_logical
SUBROUTINE ext_quilt_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_td_logical 

!--- get_dom_td_char
SUBROUTINE ext_quilt_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_td_char 

!--- put_dom_td_char
SUBROUTINE ext_quilt_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_td_char 

!--- get_var_ti_real
SUBROUTINE ext_quilt_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_ti_real 

!--- put_var_ti_real
SUBROUTINE ext_quilt_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_ti_real 

!--- get_var_ti_double
SUBROUTINE ext_quilt_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_ti_double 

!--- put_var_ti_double
SUBROUTINE ext_quilt_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_ti_double 

!--- get_var_ti_integer
SUBROUTINE ext_quilt_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_ti_integer 

!--- put_var_ti_integer
SUBROUTINE ext_quilt_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_ti_integer 

!--- get_var_ti_logical
SUBROUTINE ext_quilt_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_ti_logical 

!--- put_var_ti_logical
SUBROUTINE ext_quilt_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_ti_logical 

!--- get_var_ti_char
SUBROUTINE ext_quilt_get_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
RETURN
END SUBROUTINE ext_quilt_get_var_ti_char 

!--- put_var_ti_char
SUBROUTINE ext_quilt_put_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )

RETURN
END SUBROUTINE ext_quilt_put_var_ti_char 

!--- get_var_td_real
SUBROUTINE ext_quilt_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_td_real 

!--- put_var_td_real
SUBROUTINE ext_quilt_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_td_real 

!--- get_var_td_double
SUBROUTINE ext_quilt_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_td_double 

!--- put_var_td_double
SUBROUTINE ext_quilt_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_td_double 

!--- get_var_td_integer
SUBROUTINE ext_quilt_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount,Status)
RETURN
END SUBROUTINE ext_quilt_get_var_td_integer 

!--- put_var_td_integer
SUBROUTINE ext_quilt_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_td_integer 

!--- get_var_td_logical
SUBROUTINE ext_quilt_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_td_logical 

!--- put_var_td_logical
SUBROUTINE ext_quilt_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_td_logical 

!--- get_var_td_char
SUBROUTINE ext_quilt_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
RETURN
END SUBROUTINE ext_quilt_get_var_td_char 

!--- put_var_td_char
SUBROUTINE ext_quilt_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_td_char 

!--- read_field
SUBROUTINE ext_quilt_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
RETURN
END SUBROUTINE ext_quilt_read_field

!--- write_field
SUBROUTINE ext_quilt_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
  RETURN
END SUBROUTINE ext_quilt_write_field

!--- get_var_info  (not implemented for IntIO)
SUBROUTINE ext_quilt_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )
RETURN
END SUBROUTINE ext_quilt_get_var_info

SUBROUTINE get_mpi_comm_io_groups( retval, i )
      RETURN
END SUBROUTINE get_mpi_comm_io_groups

SUBROUTINE get_nio_tasks_in_group( retval )
      RETURN
END SUBROUTINE get_nio_tasks_in_group


