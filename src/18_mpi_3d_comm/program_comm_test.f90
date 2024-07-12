program comm_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use mpif08
    ! use MATRIX_OUTPUT
    implicit none 

    ! Parameters=============================================================
    ! Grid definition, sub_grid_y = [x_cy, y_s, z_cy]
    INTEGER, DIMENSION(3):: sub_grid_y

    ! Input Grid Data
    INTEGER:: i, num_val, arg_num
    CHARACTER(len = 32):: arg

    ! MPI Parameters
    INTEGER:: my_rank, size

    ! Body==================================================================
    ! Initialisation--------------------------------------------------------

    ! MPI Initialisation
    CALL MPI_Init()
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)

    IF (my_rank == 0) THEN
    DO i = 1, COMMAND_ARGUMENT_COUNT()  ! Should be 3
        CALL getarg(i, arg)
            READ(arg, '(I10)') sub_grid_y(i)
        END DO
    END IF
    
    CALL MPI_BCAST(sub_grid_y, 3, MPI_INTEGER, 0, MPI_WORLD)

    WRITE(*,*) sub_grid_y



end program comm_test
