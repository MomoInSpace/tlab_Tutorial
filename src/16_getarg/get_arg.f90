PROGRAM test_getarg
  implicit none
  INTEGER :: i, num_val, arg_num
  CHARACTER(len=32) :: arg

  DO i = 1, COMMAND_ARGUMENT_COUNT()
    CALL getarg(i, arg)

    READ(arg,'(I10)') num_val
    WRITE(*,*) num_val
    WRITE(*,*) arg
  END DO

END PROGRAM
