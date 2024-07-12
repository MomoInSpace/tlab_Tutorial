program matrix_multi_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    ! use MATRIX_OUTPUT
    implicit none 

    ! Grid definition
    INTEGER :: x_cy, y_s, z_cy
    ! Task Number:
    INTEGER :: m

    INTEGER :: i, num_val, arg_num
    CHARACTER(len=32) :: arg

    DO i = 1, COMMAND_ARGUMENT_COUNT()
    CALL getarg(i, arg)

    READ(arg,'(I10)') num_val
    WRITE(*,*) num_val
    WRITE(*,*) arg
    END DO


end program matrix_multi_test
