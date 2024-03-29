!WRF:MODEL_LAYER:bc_time_utilities
!

MODULE module_bc_time_utilities
  USE esmf_mod

  Type(ESMF_Time), PRIVATE, SAVE :: time_to_read_again

CONTAINS

  LOGICAL FUNCTION lbc_read_time ( xtime )
    IMPLICIT NONE
    Type (ESMF_Time), INTENT(IN) :: xtime
    IF ( xtime .LT. time_to_read_again ) THEN
      lbc_read_time = .false.
    ELSE
      lbc_read_time = .true.
    ENDIF
    RETURN
  END FUNCTION lbc_read_time

  SUBROUTINE set_time_to_read_again ( newtime )
    IMPLICIT NONE
    Type(ESMF_Time), INTENT(IN) :: newtime
    time_to_read_again = newtime
    RETURN
  END SUBROUTINE set_time_to_read_again

  SUBROUTINE get_time_to_read_again ( newtime )
    IMPLICIT NONE
    Type(ESMF_Time), INTENT(OUT) :: newtime
    newtime = time_to_read_again
    RETURN
  END SUBROUTINE get_time_to_read_again

END MODULE module_bc_time_utilities
