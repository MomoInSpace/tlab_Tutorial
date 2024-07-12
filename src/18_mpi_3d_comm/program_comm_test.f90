program comm_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use mpi_f08
    ! use MATRIX_OUTPUT
    implicit none 

    ! Parameters================================================================
    ! Grid definition, sub_grid_y = [x_cy, y_s, z_cy]
    INTEGER, DIMENSION(3):: sub_grid_y
    REAL(kind = wp), DIMENSION(:,:,:), ALLOCATABLE:: my_grid

    ! Input Grid Data
    INTEGER:: i, num_val, arg_num
    CHARACTER(len = 32):: arg

    ! MPI Parameters
    INTEGER:: size, my_rank, my_row, my_col
    INTEGER, DIMENSION(2):: dims_tasks_2d, coords
    LOGICAL, DIMENSION(2):: periods = [.false., .false.]
    TYPE(MPI_Comm):: comm_cart, comm_myRow, comm_myColumn
    

    ! Error Integer
    INTEGER:: ierr

    ! Body======================================================================
    ! Initialisation------------------------------------------------------------

    ! MPI Initialisation
    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    call MPI_Comm_size(MPI_COMM_WORLD, size)

    IF (my_rank == 0) THEN
    DO i = 1, COMMAND_ARGUMENT_COUNT()  ! Should be 3
        ! Get Arguments:
        CALL getarg(i, arg)
            READ(arg, '(I10)') sub_grid_y(i)
        END DO

        ! Check if grid has correct dimensions:
        IF (mod(size, sub_grid_y(2)) > 0) THEN
            WRITE(*,*) "ERROR: Wrong Task Number or y_s dimension, size not divisible by sub_grid_y(2)"
            CALL MPI_ABORT(MPI_COMM_WORLD, 1, ierr) 
        END IF
    END IF

    call MPI_BCAST(sub_grid_y, 3, MPI_INTEGER, 0, MPI_COMM_WORLD)

    ! Create Subgrid
    allocate(my_grid(sub_grid_y(1), sub_grid_y(2), sub_grid_y(3)), stat = ierr)
    ! if (t = ierr) print *, "u(sub_grid_y), : Allocation request denied"

    ! Create Communicator-------------------------------------------------------
    dims_tasks_2d = [sub_grid_y(2), size/sub_grid_y(2)]

    call MPI_CART_CREATE(MPI_COMM_WORLD, 2, dims_tasks_2d, periods, .true., comm_cart, ierr)
    call MPI_Comm_rank(comm_cart, my_rank)
    call MPI_CART_COORDS(comm_cart, my_rank, 2, coords, ierr)
    call MPI_Comm_split(comm_cart, coords(1), coords(2), comm_myColumn, ierr)
    call MPI_Comm_split(comm_cart, coords(2), coords(1), comm_myRow, ierr)
    call MPI_Comm_free(comm_cart, ierr)





    ! Cleanup-------------------------------------------------------------------
    if (allocated(my_grid)) then
        deallocate(my_grid, stat = ierr)
    end if
    ! if (t = ierr) print *, "u(sub_grid_y), : Deallocation request denied"
    

    


end program comm_test
