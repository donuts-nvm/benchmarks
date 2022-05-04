!WRF:MEDIATION_LAYER:
!

SUBROUTINE med_initialdata_input_ptr ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE (domain) , POINTER :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTERFACE 
      SUBROUTINE med_initialdata_input ( grid , config_flags )
         USE module_domain
         USE module_configure
         TYPE (domain) :: grid
         TYPE (grid_config_rec_type) :: config_flags
      END SUBROUTINE med_initialdata_input
   END INTERFACE
   CALL  med_initialdata_input ( grid , config_flags )
END SUBROUTINE med_initialdata_input_ptr

SUBROUTINE med_initialdata_input ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE esmf_mod

   IMPLICIT NONE


  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid )  ! comes from module_start in appropriate dyn_ directory
       USE module_domain
       TYPE (domain) grid
     END SUBROUTINE start_domain
   END INTERFACE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                :: fid , ierr , myproc
   CHARACTER (LEN=80)     :: inpname , rstname, timestr
   CHARACTER (LEN=80)     :: message
   LOGICAL                :: restart
   TYPE(ESMF_Time)        :: CurrTime


   CALL get_restart( restart )
   IF ( .NOT. restart ) THEN
     !  Initialize the mother domain.
     CALL start_timing
     grid%input_from_file = .true.
     IF ( grid%input_from_file ) THEN

        CALL       wrf_debug ( 1 , 'wrf main: calling open_r_dataset for wrfinput' )

! typically <date> will not be part of input_inname but allow for it
        CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
        CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
        CALL construct_filename2a ( inpname , config_flags%input_inname , grid%id , 2 , timestr )

        CALL open_r_dataset ( fid, TRIM(inpname) , grid , config_flags , "DATASET=INPUT", ierr )
        IF ( ierr .NE. 0 ) THEN
          WRITE( wrf_err_message , * ) 'program wrf: error opening ',TRIM(inpname),' for reading ierr=',ierr
          CALL WRF_ERROR_FATAL ( wrf_err_message )
        ENDIF
        CALL       wrf_debug ( 100 , 'wrf: calling input_model_input' )
        CALL input_model_input ( fid ,   grid , config_flags , ierr )
        CALL       wrf_debug ( 100 , 'wrf: back from input_model_input' )
        CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )
     ENDIF
     CALL start_domain ( grid )
   ELSE
     CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
     CALL construct_filename2a ( rstname , config_flags%rst_inname , grid%id , 2 , timestr )

     WRITE(message,*)'opening ',TRIM(rstname),' for reading'
     CALL wrf_debug ( 1 , message )
     CALL open_r_dataset ( fid , TRIM(rstname) , grid , config_flags , "DATASET=RESTART", ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE( message , '("program wrf: error opening ",A32," for reading")') TRIM(rstname)
       CALL WRF_ERROR_FATAL ( message )
     ENDIF
     CALL input_restart ( fid,   grid , config_flags , ierr )
     CALL close_dataset ( fid , config_flags , "DATASET=RESTART" )
     CALL start_domain ( grid )
   ENDIF

   RETURN
END SUBROUTINE med_initialdata_input

SUBROUTINE med_shutdown_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   CHARACTER (LEN=80)      :: message
   INTEGER                 :: ierr

   IF ( grid%oid > 0 ) CALL close_dataset ( grid%oid , config_flags , "DATASET=HISTORY" )
   IF ( grid%auxhist1_oid > 0 ) CALL close_dataset ( grid%auxhist1_oid , config_flags , "DATASET=AUXHIST1" )
   IF ( grid%auxhist2_oid > 0 ) CALL close_dataset ( grid%auxhist2_oid , config_flags , "DATASET=AUXHIST2" )
   IF ( grid%auxhist3_oid > 0 ) CALL close_dataset ( grid%auxhist3_oid , config_flags , "DATASET=AUXHIST3" )
   IF ( grid%auxhist4_oid > 0 ) CALL close_dataset ( grid%auxhist4_oid , config_flags , "DATASET=AUXHIST4" )
   IF ( grid%auxhist5_oid > 0 ) CALL close_dataset ( grid%auxhist5_oid , config_flags , "DATASET=AUXHIST5" )

   CALL wrf_ioexit( ierr )    ! shut down the quilt I/O

   RETURN

END SUBROUTINE med_shutdown_io

SUBROUTINE med_add_config_info_to_grid ( grid )

   USE module_domain
   USE module_configure
 
   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/config_assigns.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! Contains config assign statements for module_domain.F.
 grid % run_days                   = model_config_rec % run_days 
 grid % run_hours                  = model_config_rec % run_hours 
 grid % run_minutes                = model_config_rec % run_minutes 
 grid % run_seconds                = model_config_rec % run_seconds 
 grid % start_year                 = model_config_rec % start_year (grid%id)
 grid % start_month                = model_config_rec % start_month (grid%id)
 grid % start_day                  = model_config_rec % start_day (grid%id)
 grid % start_hour                 = model_config_rec % start_hour (grid%id)
 grid % start_minute               = model_config_rec % start_minute (grid%id)
 grid % start_second               = model_config_rec % start_second (grid%id)
 grid % end_year                   = model_config_rec % end_year (grid%id)
 grid % end_month                  = model_config_rec % end_month (grid%id)
 grid % end_day                    = model_config_rec % end_day (grid%id)
 grid % end_hour                   = model_config_rec % end_hour (grid%id)
 grid % end_minute                 = model_config_rec % end_minute (grid%id)
 grid % end_second                 = model_config_rec % end_second (grid%id)
 grid % interval_seconds           = model_config_rec % interval_seconds 
 grid % input_from_file            = model_config_rec % input_from_file (grid%id)
 grid % history_interval           = model_config_rec % history_interval (grid%id)
 grid % frames_per_outfile         = model_config_rec % frames_per_outfile (grid%id)
 grid % restart                    = model_config_rec % restart 
 grid % restart_interval           = model_config_rec % restart_interval 
 grid % io_form_input              = model_config_rec % io_form_input 
 grid % io_form_history            = model_config_rec % io_form_history 
 grid % io_form_restart            = model_config_rec % io_form_restart 
 grid % io_form_boundary           = model_config_rec % io_form_boundary 
 grid % debug_level                = model_config_rec % debug_level 
 grid % history_outname            = model_config_rec % history_outname 
 grid % auxhist1_outname           = model_config_rec % auxhist1_outname 
 grid % auxhist2_outname           = model_config_rec % auxhist2_outname 
 grid % auxhist3_outname           = model_config_rec % auxhist3_outname 
 grid % auxhist4_outname           = model_config_rec % auxhist4_outname 
 grid % auxhist5_outname           = model_config_rec % auxhist5_outname 
 grid % history_inname             = model_config_rec % history_inname 
 grid % auxhist1_inname            = model_config_rec % auxhist1_inname 
 grid % auxhist2_inname            = model_config_rec % auxhist2_inname 
 grid % auxhist3_inname            = model_config_rec % auxhist3_inname 
 grid % auxhist4_inname            = model_config_rec % auxhist4_inname 
 grid % auxhist5_inname            = model_config_rec % auxhist5_inname 
 grid % history_interval_mo        = model_config_rec % history_interval_mo (grid%id)
 grid % history_interval_d         = model_config_rec % history_interval_d (grid%id)
 grid % history_interval_h         = model_config_rec % history_interval_h (grid%id)
 grid % history_interval_m         = model_config_rec % history_interval_m (grid%id)
 grid % history_interval_s         = model_config_rec % history_interval_s (grid%id)
 grid % inputout_interval_mo       = model_config_rec % inputout_interval_mo (grid%id)
 grid % inputout_interval_d        = model_config_rec % inputout_interval_d (grid%id)
 grid % inputout_interval_h        = model_config_rec % inputout_interval_h (grid%id)
 grid % inputout_interval_m        = model_config_rec % inputout_interval_m (grid%id)
 grid % inputout_interval_s        = model_config_rec % inputout_interval_s (grid%id)
 grid % inputout_interval          = model_config_rec % inputout_interval (grid%id)
 grid % auxhist1_interval_mo       = model_config_rec % auxhist1_interval_mo (grid%id)
 grid % auxhist1_interval_d        = model_config_rec % auxhist1_interval_d (grid%id)
 grid % auxhist1_interval_h        = model_config_rec % auxhist1_interval_h (grid%id)
 grid % auxhist1_interval_m        = model_config_rec % auxhist1_interval_m (grid%id)
 grid % auxhist1_interval_s        = model_config_rec % auxhist1_interval_s (grid%id)
 grid % auxhist1_interval          = model_config_rec % auxhist1_interval (grid%id)
 grid % auxhist2_interval_mo       = model_config_rec % auxhist2_interval_mo (grid%id)
 grid % auxhist2_interval_d        = model_config_rec % auxhist2_interval_d (grid%id)
 grid % auxhist2_interval_h        = model_config_rec % auxhist2_interval_h (grid%id)
 grid % auxhist2_interval_m        = model_config_rec % auxhist2_interval_m (grid%id)
 grid % auxhist2_interval_s        = model_config_rec % auxhist2_interval_s (grid%id)
 grid % auxhist2_interval          = model_config_rec % auxhist2_interval (grid%id)
 grid % auxhist3_interval_mo       = model_config_rec % auxhist3_interval_mo (grid%id)
 grid % auxhist3_interval_d        = model_config_rec % auxhist3_interval_d (grid%id)
 grid % auxhist3_interval_h        = model_config_rec % auxhist3_interval_h (grid%id)
 grid % auxhist3_interval_m        = model_config_rec % auxhist3_interval_m (grid%id)
 grid % auxhist3_interval_s        = model_config_rec % auxhist3_interval_s (grid%id)
 grid % auxhist3_interval          = model_config_rec % auxhist3_interval (grid%id)
 grid % auxhist4_interval_mo       = model_config_rec % auxhist4_interval_mo (grid%id)
 grid % auxhist4_interval_d        = model_config_rec % auxhist4_interval_d (grid%id)
 grid % auxhist4_interval_h        = model_config_rec % auxhist4_interval_h (grid%id)
 grid % auxhist4_interval_m        = model_config_rec % auxhist4_interval_m (grid%id)
 grid % auxhist4_interval_s        = model_config_rec % auxhist4_interval_s (grid%id)
 grid % auxhist4_interval          = model_config_rec % auxhist4_interval (grid%id)
 grid % auxhist5_interval_mo       = model_config_rec % auxhist5_interval_mo (grid%id)
 grid % auxhist5_interval_d        = model_config_rec % auxhist5_interval_d (grid%id)
 grid % auxhist5_interval_h        = model_config_rec % auxhist5_interval_h (grid%id)
 grid % auxhist5_interval_m        = model_config_rec % auxhist5_interval_m (grid%id)
 grid % auxhist5_interval_s        = model_config_rec % auxhist5_interval_s (grid%id)
 grid % auxhist5_interval          = model_config_rec % auxhist5_interval (grid%id)
 grid % auxinput1_interval_mo      = model_config_rec % auxinput1_interval_mo (grid%id)
 grid % auxinput1_interval_d       = model_config_rec % auxinput1_interval_d (grid%id)
 grid % auxinput1_interval_h       = model_config_rec % auxinput1_interval_h (grid%id)
 grid % auxinput1_interval_m       = model_config_rec % auxinput1_interval_m (grid%id)
 grid % auxinput1_interval_s       = model_config_rec % auxinput1_interval_s (grid%id)
 grid % auxinput1_interval         = model_config_rec % auxinput1_interval (grid%id)
 grid % auxinput2_interval_mo      = model_config_rec % auxinput2_interval_mo (grid%id)
 grid % auxinput2_interval_d       = model_config_rec % auxinput2_interval_d (grid%id)
 grid % auxinput2_interval_h       = model_config_rec % auxinput2_interval_h (grid%id)
 grid % auxinput2_interval_m       = model_config_rec % auxinput2_interval_m (grid%id)
 grid % auxinput2_interval_s       = model_config_rec % auxinput2_interval_s (grid%id)
 grid % auxinput2_interval         = model_config_rec % auxinput2_interval (grid%id)
 grid % auxinput3_interval_mo      = model_config_rec % auxinput3_interval_mo (grid%id)
 grid % auxinput3_interval_d       = model_config_rec % auxinput3_interval_d (grid%id)
 grid % auxinput3_interval_h       = model_config_rec % auxinput3_interval_h (grid%id)
 grid % auxinput3_interval_m       = model_config_rec % auxinput3_interval_m (grid%id)
 grid % auxinput3_interval_s       = model_config_rec % auxinput3_interval_s (grid%id)
 grid % auxinput3_interval         = model_config_rec % auxinput3_interval (grid%id)
 grid % auxinput4_interval_mo      = model_config_rec % auxinput4_interval_mo (grid%id)
 grid % auxinput4_interval_d       = model_config_rec % auxinput4_interval_d (grid%id)
 grid % auxinput4_interval_h       = model_config_rec % auxinput4_interval_h (grid%id)
 grid % auxinput4_interval_m       = model_config_rec % auxinput4_interval_m (grid%id)
 grid % auxinput4_interval_s       = model_config_rec % auxinput4_interval_s (grid%id)
 grid % auxinput4_interval         = model_config_rec % auxinput4_interval (grid%id)
 grid % auxinput5_interval_mo      = model_config_rec % auxinput5_interval_mo (grid%id)
 grid % auxinput5_interval_d       = model_config_rec % auxinput5_interval_d (grid%id)
 grid % auxinput5_interval_h       = model_config_rec % auxinput5_interval_h (grid%id)
 grid % auxinput5_interval_m       = model_config_rec % auxinput5_interval_m (grid%id)
 grid % auxinput5_interval_s       = model_config_rec % auxinput5_interval_s (grid%id)
 grid % auxinput5_interval         = model_config_rec % auxinput5_interval (grid%id)
 grid % restart_interval_mo        = model_config_rec % restart_interval_mo 
 grid % restart_interval_d         = model_config_rec % restart_interval_d 
 grid % restart_interval_h         = model_config_rec % restart_interval_h 
 grid % restart_interval_m         = model_config_rec % restart_interval_m 
 grid % restart_interval_s         = model_config_rec % restart_interval_s 
 grid % history_begin_y            = model_config_rec % history_begin_y (grid%id)
 grid % history_begin_mo           = model_config_rec % history_begin_mo (grid%id)
 grid % history_begin_d            = model_config_rec % history_begin_d (grid%id)
 grid % history_begin_h            = model_config_rec % history_begin_h (grid%id)
 grid % history_begin_m            = model_config_rec % history_begin_m (grid%id)
 grid % history_begin_s            = model_config_rec % history_begin_s (grid%id)
 grid % inputout_begin_y           = model_config_rec % inputout_begin_y (grid%id)
 grid % inputout_begin_mo          = model_config_rec % inputout_begin_mo (grid%id)
 grid % inputout_begin_d           = model_config_rec % inputout_begin_d (grid%id)
 grid % inputout_begin_h           = model_config_rec % inputout_begin_h (grid%id)
 grid % inputout_begin_m           = model_config_rec % inputout_begin_m (grid%id)
 grid % inputout_begin_s           = model_config_rec % inputout_begin_s (grid%id)
 grid % auxhist1_begin_y           = model_config_rec % auxhist1_begin_y (grid%id)
 grid % auxhist1_begin_mo          = model_config_rec % auxhist1_begin_mo (grid%id)
 grid % auxhist1_begin_d           = model_config_rec % auxhist1_begin_d (grid%id)
 grid % auxhist1_begin_h           = model_config_rec % auxhist1_begin_h (grid%id)
 grid % auxhist1_begin_m           = model_config_rec % auxhist1_begin_m (grid%id)
 grid % auxhist1_begin_s           = model_config_rec % auxhist1_begin_s (grid%id)
 grid % auxhist2_begin_y           = model_config_rec % auxhist2_begin_y (grid%id)
 grid % auxhist2_begin_mo          = model_config_rec % auxhist2_begin_mo (grid%id)
 grid % auxhist2_begin_d           = model_config_rec % auxhist2_begin_d (grid%id)
 grid % auxhist2_begin_h           = model_config_rec % auxhist2_begin_h (grid%id)
 grid % auxhist2_begin_m           = model_config_rec % auxhist2_begin_m (grid%id)
 grid % auxhist2_begin_s           = model_config_rec % auxhist2_begin_s (grid%id)
 grid % auxhist3_begin_y           = model_config_rec % auxhist3_begin_y (grid%id)
 grid % auxhist3_begin_mo          = model_config_rec % auxhist3_begin_mo (grid%id)
 grid % auxhist3_begin_d           = model_config_rec % auxhist3_begin_d (grid%id)
 grid % auxhist3_begin_h           = model_config_rec % auxhist3_begin_h (grid%id)
 grid % auxhist3_begin_m           = model_config_rec % auxhist3_begin_m (grid%id)
 grid % auxhist3_begin_s           = model_config_rec % auxhist3_begin_s (grid%id)
 grid % auxhist4_begin_y           = model_config_rec % auxhist4_begin_y (grid%id)
 grid % auxhist4_begin_mo          = model_config_rec % auxhist4_begin_mo (grid%id)
 grid % auxhist4_begin_d           = model_config_rec % auxhist4_begin_d (grid%id)
 grid % auxhist4_begin_h           = model_config_rec % auxhist4_begin_h (grid%id)
 grid % auxhist4_begin_m           = model_config_rec % auxhist4_begin_m (grid%id)
 grid % auxhist4_begin_s           = model_config_rec % auxhist4_begin_s (grid%id)
 grid % auxhist5_begin_y           = model_config_rec % auxhist5_begin_y (grid%id)
 grid % auxhist5_begin_mo          = model_config_rec % auxhist5_begin_mo (grid%id)
 grid % auxhist5_begin_d           = model_config_rec % auxhist5_begin_d (grid%id)
 grid % auxhist5_begin_h           = model_config_rec % auxhist5_begin_h (grid%id)
 grid % auxhist5_begin_m           = model_config_rec % auxhist5_begin_m (grid%id)
 grid % auxhist5_begin_s           = model_config_rec % auxhist5_begin_s (grid%id)
 grid % auxinput1_begin_y          = model_config_rec % auxinput1_begin_y (grid%id)
 grid % auxinput1_begin_mo         = model_config_rec % auxinput1_begin_mo (grid%id)
 grid % auxinput1_begin_d          = model_config_rec % auxinput1_begin_d (grid%id)
 grid % auxinput1_begin_h          = model_config_rec % auxinput1_begin_h (grid%id)
 grid % auxinput1_begin_m          = model_config_rec % auxinput1_begin_m (grid%id)
 grid % auxinput1_begin_s          = model_config_rec % auxinput1_begin_s (grid%id)
 grid % auxinput2_begin_y          = model_config_rec % auxinput2_begin_y (grid%id)
 grid % auxinput2_begin_mo         = model_config_rec % auxinput2_begin_mo (grid%id)
 grid % auxinput2_begin_d          = model_config_rec % auxinput2_begin_d (grid%id)
 grid % auxinput2_begin_h          = model_config_rec % auxinput2_begin_h (grid%id)
 grid % auxinput2_begin_m          = model_config_rec % auxinput2_begin_m (grid%id)
 grid % auxinput2_begin_s          = model_config_rec % auxinput2_begin_s (grid%id)
 grid % auxinput3_begin_y          = model_config_rec % auxinput3_begin_y (grid%id)
 grid % auxinput3_begin_mo         = model_config_rec % auxinput3_begin_mo (grid%id)
 grid % auxinput3_begin_d          = model_config_rec % auxinput3_begin_d (grid%id)
 grid % auxinput3_begin_h          = model_config_rec % auxinput3_begin_h (grid%id)
 grid % auxinput3_begin_m          = model_config_rec % auxinput3_begin_m (grid%id)
 grid % auxinput3_begin_s          = model_config_rec % auxinput3_begin_s (grid%id)
 grid % auxinput4_begin_y          = model_config_rec % auxinput4_begin_y (grid%id)
 grid % auxinput4_begin_mo         = model_config_rec % auxinput4_begin_mo (grid%id)
 grid % auxinput4_begin_d          = model_config_rec % auxinput4_begin_d (grid%id)
 grid % auxinput4_begin_h          = model_config_rec % auxinput4_begin_h (grid%id)
 grid % auxinput4_begin_m          = model_config_rec % auxinput4_begin_m (grid%id)
 grid % auxinput4_begin_s          = model_config_rec % auxinput4_begin_s (grid%id)
 grid % auxinput5_begin_y          = model_config_rec % auxinput5_begin_y (grid%id)
 grid % auxinput5_begin_mo         = model_config_rec % auxinput5_begin_mo (grid%id)
 grid % auxinput5_begin_d          = model_config_rec % auxinput5_begin_d (grid%id)
 grid % auxinput5_begin_h          = model_config_rec % auxinput5_begin_h (grid%id)
 grid % auxinput5_begin_m          = model_config_rec % auxinput5_begin_m (grid%id)
 grid % auxinput5_begin_s          = model_config_rec % auxinput5_begin_s (grid%id)
 grid % restart_begin_y            = model_config_rec % restart_begin_y 
 grid % restart_begin_mo           = model_config_rec % restart_begin_mo 
 grid % restart_begin_d            = model_config_rec % restart_begin_d 
 grid % restart_begin_h            = model_config_rec % restart_begin_h 
 grid % restart_begin_m            = model_config_rec % restart_begin_m 
 grid % restart_begin_s            = model_config_rec % restart_begin_s 
 grid % history_end_y              = model_config_rec % history_end_y (grid%id)
 grid % history_end_mo             = model_config_rec % history_end_mo (grid%id)
 grid % history_end_d              = model_config_rec % history_end_d (grid%id)
 grid % history_end_h              = model_config_rec % history_end_h (grid%id)
 grid % history_end_m              = model_config_rec % history_end_m (grid%id)
 grid % history_end_s              = model_config_rec % history_end_s (grid%id)
 grid % inputout_end_y             = model_config_rec % inputout_end_y (grid%id)
 grid % inputout_end_mo            = model_config_rec % inputout_end_mo (grid%id)
 grid % inputout_end_d             = model_config_rec % inputout_end_d (grid%id)
 grid % inputout_end_h             = model_config_rec % inputout_end_h (grid%id)
 grid % inputout_end_m             = model_config_rec % inputout_end_m (grid%id)
 grid % inputout_end_s             = model_config_rec % inputout_end_s (grid%id)
 grid % auxhist1_end_y             = model_config_rec % auxhist1_end_y (grid%id)
 grid % auxhist1_end_mo            = model_config_rec % auxhist1_end_mo (grid%id)
 grid % auxhist1_end_d             = model_config_rec % auxhist1_end_d (grid%id)
 grid % auxhist1_end_h             = model_config_rec % auxhist1_end_h (grid%id)
 grid % auxhist1_end_m             = model_config_rec % auxhist1_end_m (grid%id)
 grid % auxhist1_end_s             = model_config_rec % auxhist1_end_s (grid%id)
 grid % auxhist2_end_y             = model_config_rec % auxhist2_end_y (grid%id)
 grid % auxhist2_end_mo            = model_config_rec % auxhist2_end_mo (grid%id)
 grid % auxhist2_end_d             = model_config_rec % auxhist2_end_d (grid%id)
 grid % auxhist2_end_h             = model_config_rec % auxhist2_end_h (grid%id)
 grid % auxhist2_end_m             = model_config_rec % auxhist2_end_m (grid%id)
 grid % auxhist2_end_s             = model_config_rec % auxhist2_end_s (grid%id)
 grid % auxhist3_end_y             = model_config_rec % auxhist3_end_y (grid%id)
 grid % auxhist3_end_mo            = model_config_rec % auxhist3_end_mo (grid%id)
 grid % auxhist3_end_d             = model_config_rec % auxhist3_end_d (grid%id)
 grid % auxhist3_end_h             = model_config_rec % auxhist3_end_h (grid%id)
 grid % auxhist3_end_m             = model_config_rec % auxhist3_end_m (grid%id)
 grid % auxhist3_end_s             = model_config_rec % auxhist3_end_s (grid%id)
 grid % auxhist4_end_y             = model_config_rec % auxhist4_end_y (grid%id)
 grid % auxhist4_end_mo            = model_config_rec % auxhist4_end_mo (grid%id)
 grid % auxhist4_end_d             = model_config_rec % auxhist4_end_d (grid%id)
 grid % auxhist4_end_h             = model_config_rec % auxhist4_end_h (grid%id)
 grid % auxhist4_end_m             = model_config_rec % auxhist4_end_m (grid%id)
 grid % auxhist4_end_s             = model_config_rec % auxhist4_end_s (grid%id)
 grid % auxhist5_end_y             = model_config_rec % auxhist5_end_y (grid%id)
 grid % auxhist5_end_mo            = model_config_rec % auxhist5_end_mo (grid%id)
 grid % auxhist5_end_d             = model_config_rec % auxhist5_end_d (grid%id)
 grid % auxhist5_end_h             = model_config_rec % auxhist5_end_h (grid%id)
 grid % auxhist5_end_m             = model_config_rec % auxhist5_end_m (grid%id)
 grid % auxhist5_end_s             = model_config_rec % auxhist5_end_s (grid%id)
 grid % auxinput1_end_y            = model_config_rec % auxinput1_end_y (grid%id)
 grid % auxinput1_end_mo           = model_config_rec % auxinput1_end_mo (grid%id)
 grid % auxinput1_end_d            = model_config_rec % auxinput1_end_d (grid%id)
 grid % auxinput1_end_h            = model_config_rec % auxinput1_end_h (grid%id)
 grid % auxinput1_end_m            = model_config_rec % auxinput1_end_m (grid%id)
 grid % auxinput1_end_s            = model_config_rec % auxinput1_end_s (grid%id)
 grid % auxinput2_end_y            = model_config_rec % auxinput2_end_y (grid%id)
 grid % auxinput2_end_mo           = model_config_rec % auxinput2_end_mo (grid%id)
 grid % auxinput2_end_d            = model_config_rec % auxinput2_end_d (grid%id)
 grid % auxinput2_end_h            = model_config_rec % auxinput2_end_h (grid%id)
 grid % auxinput2_end_m            = model_config_rec % auxinput2_end_m (grid%id)
 grid % auxinput2_end_s            = model_config_rec % auxinput2_end_s (grid%id)
 grid % auxinput3_end_y            = model_config_rec % auxinput3_end_y (grid%id)
 grid % auxinput3_end_mo           = model_config_rec % auxinput3_end_mo (grid%id)
 grid % auxinput3_end_d            = model_config_rec % auxinput3_end_d (grid%id)
 grid % auxinput3_end_h            = model_config_rec % auxinput3_end_h (grid%id)
 grid % auxinput3_end_m            = model_config_rec % auxinput3_end_m (grid%id)
 grid % auxinput3_end_s            = model_config_rec % auxinput3_end_s (grid%id)
 grid % auxinput4_end_y            = model_config_rec % auxinput4_end_y (grid%id)
 grid % auxinput4_end_mo           = model_config_rec % auxinput4_end_mo (grid%id)
 grid % auxinput4_end_d            = model_config_rec % auxinput4_end_d (grid%id)
 grid % auxinput4_end_h            = model_config_rec % auxinput4_end_h (grid%id)
 grid % auxinput4_end_m            = model_config_rec % auxinput4_end_m (grid%id)
 grid % auxinput4_end_s            = model_config_rec % auxinput4_end_s (grid%id)
 grid % auxinput5_end_y            = model_config_rec % auxinput5_end_y (grid%id)
 grid % auxinput5_end_mo           = model_config_rec % auxinput5_end_mo (grid%id)
 grid % auxinput5_end_d            = model_config_rec % auxinput5_end_d (grid%id)
 grid % auxinput5_end_h            = model_config_rec % auxinput5_end_h (grid%id)
 grid % auxinput5_end_m            = model_config_rec % auxinput5_end_m (grid%id)
 grid % auxinput5_end_s            = model_config_rec % auxinput5_end_s (grid%id)
 grid % io_form_auxinput1          = model_config_rec % io_form_auxinput1 
 grid % io_form_auxinput2          = model_config_rec % io_form_auxinput2 
 grid % io_form_auxinput3          = model_config_rec % io_form_auxinput3 
 grid % io_form_auxinput4          = model_config_rec % io_form_auxinput4 
 grid % io_form_auxinput5          = model_config_rec % io_form_auxinput5 
 grid % io_form_auxhist1           = model_config_rec % io_form_auxhist1 
 grid % io_form_auxhist2           = model_config_rec % io_form_auxhist2 
 grid % io_form_auxhist3           = model_config_rec % io_form_auxhist3 
 grid % io_form_auxhist4           = model_config_rec % io_form_auxhist4 
 grid % io_form_auxhist5           = model_config_rec % io_form_auxhist5 
 grid % julyr                      = model_config_rec % julyr (grid%id)
 grid % julday                     = model_config_rec % julday (grid%id)
 grid % gmt                        = model_config_rec % gmt (grid%id)
 grid % input_inname               = model_config_rec % input_inname 
 grid % input_outname              = model_config_rec % input_outname 
 grid % bdy_inname                 = model_config_rec % bdy_inname 
 grid % bdy_outname                = model_config_rec % bdy_outname 
 grid % rst_inname                 = model_config_rec % rst_inname 
 grid % rst_outname                = model_config_rec % rst_outname 
 grid % write_input                = model_config_rec % write_input 
 grid % write_restart_at_0h        = model_config_rec % write_restart_at_0h 
 grid % time_step                  = model_config_rec % time_step 
 grid % time_step_fract_num        = model_config_rec % time_step_fract_num 
 grid % time_step_fract_den        = model_config_rec % time_step_fract_den 
 grid % max_dom                    = model_config_rec % max_dom 
 grid % s_we                       = model_config_rec % s_we (grid%id)
 grid % e_we                       = model_config_rec % e_we (grid%id)
 grid % s_sn                       = model_config_rec % s_sn (grid%id)
 grid % e_sn                       = model_config_rec % e_sn (grid%id)
 grid % s_vert                     = model_config_rec % s_vert (grid%id)
 grid % e_vert                     = model_config_rec % e_vert (grid%id)
 grid % dx                         = model_config_rec % dx (grid%id)
 grid % dy                         = model_config_rec % dy (grid%id)
 grid % grid_id                    = model_config_rec % grid_id (grid%id)
 grid % parent_id                  = model_config_rec % parent_id (grid%id)
 grid % level                      = model_config_rec % level (grid%id)
 grid % i_parent_start             = model_config_rec % i_parent_start (grid%id)
 grid % j_parent_start             = model_config_rec % j_parent_start (grid%id)
 grid % parent_grid_ratio          = model_config_rec % parent_grid_ratio (grid%id)
 grid % parent_time_step_ratio     = model_config_rec % parent_time_step_ratio (grid%id)
 grid % feedback                   = model_config_rec % feedback 
 grid % smooth_option              = model_config_rec % smooth_option 
 grid % ztop                       = model_config_rec % ztop (grid%id)
 grid % moad_grid_ratio            = model_config_rec % moad_grid_ratio (grid%id)
 grid % moad_time_step_ratio       = model_config_rec % moad_time_step_ratio (grid%id)
 grid % shw                        = model_config_rec % shw (grid%id)
 grid % tile_sz_x                  = model_config_rec % tile_sz_x 
 grid % tile_sz_y                  = model_config_rec % tile_sz_y 
 grid % numtiles                   = model_config_rec % numtiles 
 grid % nproc_x                    = model_config_rec % nproc_x 
 grid % nproc_y                    = model_config_rec % nproc_y 
 grid % irand                      = model_config_rec % irand 
 grid % dt                         = model_config_rec % dt (grid%id)
 grid % mp_physics                 = model_config_rec % mp_physics (grid%id)
 grid % ra_lw_physics              = model_config_rec % ra_lw_physics (grid%id)
 grid % ra_sw_physics              = model_config_rec % ra_sw_physics (grid%id)
 grid % radt                       = model_config_rec % radt (grid%id)
 grid % sf_sfclay_physics          = model_config_rec % sf_sfclay_physics (grid%id)
 grid % sf_surface_physics         = model_config_rec % sf_surface_physics (grid%id)
 grid % bl_pbl_physics             = model_config_rec % bl_pbl_physics (grid%id)
 grid % bldt                       = model_config_rec % bldt (grid%id)
 grid % cu_physics                 = model_config_rec % cu_physics (grid%id)
 grid % cudt                       = model_config_rec % cudt (grid%id)
 grid % gsmdt                      = model_config_rec % gsmdt (grid%id)
 grid % isfflx                     = model_config_rec % isfflx 
 grid % ifsnow                     = model_config_rec % ifsnow 
 grid % icloud                     = model_config_rec % icloud 
 grid % surface_input_source       = model_config_rec % surface_input_source 
 grid % num_soil_layers            = model_config_rec % num_soil_layers 
 grid % maxiens                    = model_config_rec % maxiens 
 grid % maxens                     = model_config_rec % maxens 
 grid % maxens2                    = model_config_rec % maxens2 
 grid % maxens3                    = model_config_rec % maxens3 
 grid % ensdim                     = model_config_rec % ensdim 
 grid % chem_opt                   = model_config_rec % chem_opt (grid%id)
 grid % num_land_cat               = model_config_rec % num_land_cat 
 grid % num_soil_cat               = model_config_rec % num_soil_cat 
 grid % dyn_opt                    = model_config_rec % dyn_opt 
 grid % rk_ord                     = model_config_rec % rk_ord 
 grid % w_damping                  = model_config_rec % w_damping 
 grid % diff_opt                   = model_config_rec % diff_opt 
 grid % km_opt                     = model_config_rec % km_opt 
 grid % damp_opt                   = model_config_rec % damp_opt 
 grid % zdamp                      = model_config_rec % zdamp (grid%id)
 grid % dampcoef                   = model_config_rec % dampcoef (grid%id)
 grid % khdif                      = model_config_rec % khdif (grid%id)
 grid % kvdif                      = model_config_rec % kvdif (grid%id)
 grid % smdiv                      = model_config_rec % smdiv (grid%id)
 grid % emdiv                      = model_config_rec % emdiv (grid%id)
 grid % epssm                      = model_config_rec % epssm (grid%id)
 grid % non_hydrostatic            = model_config_rec % non_hydrostatic (grid%id)
 grid % time_step_sound            = model_config_rec % time_step_sound (grid%id)
 grid % h_mom_adv_order            = model_config_rec % h_mom_adv_order (grid%id)
 grid % v_mom_adv_order            = model_config_rec % v_mom_adv_order (grid%id)
 grid % h_sca_adv_order            = model_config_rec % h_sca_adv_order (grid%id)
 grid % v_sca_adv_order            = model_config_rec % v_sca_adv_order (grid%id)
 grid % top_radiation              = model_config_rec % top_radiation (grid%id)
 grid % mix_cr_len                 = model_config_rec % mix_cr_len (grid%id)
 grid % tke_upper_bound            = model_config_rec % tke_upper_bound (grid%id)
 grid % kh_tke_upper_bound         = model_config_rec % kh_tke_upper_bound (grid%id)
 grid % kv_tke_upper_bound         = model_config_rec % kv_tke_upper_bound (grid%id)
 grid % tke_drag_coefficient       = model_config_rec % tke_drag_coefficient (grid%id)
 grid % tke_heat_flux              = model_config_rec % tke_heat_flux (grid%id)
 grid % pert_coriolis              = model_config_rec % pert_coriolis (grid%id)
 grid % spec_bdy_width             = model_config_rec % spec_bdy_width 
 grid % spec_zone                  = model_config_rec % spec_zone 
 grid % relax_zone                 = model_config_rec % relax_zone 
 grid % specified                  = model_config_rec % specified (grid%id)
 grid % periodic_x                 = model_config_rec % periodic_x (grid%id)
 grid % symmetric_xs               = model_config_rec % symmetric_xs (grid%id)
 grid % symmetric_xe               = model_config_rec % symmetric_xe (grid%id)
 grid % open_xs                    = model_config_rec % open_xs (grid%id)
 grid % open_xe                    = model_config_rec % open_xe (grid%id)
 grid % periodic_y                 = model_config_rec % periodic_y (grid%id)
 grid % symmetric_ys               = model_config_rec % symmetric_ys (grid%id)
 grid % symmetric_ye               = model_config_rec % symmetric_ye (grid%id)
 grid % open_ys                    = model_config_rec % open_ys (grid%id)
 grid % open_ye                    = model_config_rec % open_ye (grid%id)
 grid % nested                     = model_config_rec % nested (grid%id)
 grid % real_data_init_type        = model_config_rec % real_data_init_type 
 grid % cen_lat                    = model_config_rec % cen_lat (grid%id)
 grid % cen_lon                    = model_config_rec % cen_lon (grid%id)
 grid % truelat1                   = model_config_rec % truelat1 (grid%id)
 grid % truelat2                   = model_config_rec % truelat2 (grid%id)
 grid % moad_cen_lat               = model_config_rec % moad_cen_lat (grid%id)
 grid % stand_lon                  = model_config_rec % stand_lon (grid%id)
 grid % bdyfrq                     = model_config_rec % bdyfrq (grid%id)
 grid % iswater                    = model_config_rec % iswater (grid%id)
 grid % isice                      = model_config_rec % isice (grid%id)
 grid % isurban                    = model_config_rec % isurban (grid%id)
 grid % isoilwater                 = model_config_rec % isoilwater (grid%id)
 grid % map_proj                   = model_config_rec % map_proj (grid%id)
!ENDOFREGISTRYGENERATEDINCLUDE

   RETURN

END SUBROUTINE med_add_config_info_to_grid

