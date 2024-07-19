program comm_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use mpi_f08
    use grid_utils
    use subgrid_handler
    ! use MATRIX_OUTPUT
    implicit none 

    ! Parameters================================================================
    ! Grid definition, sub_grid_y = [x_cy*, y_s, z_cy*]
    ! INTEGER, DIMENSION(3), target:: sub_grid_y

    integer                             :: state, send_num
    type(Subgrid)                      :: subgrid_handler, testgrid_handler
    integer, dimension(3)               :: subgrid_xyz_dims 
    real(kind = wp), asynchronous, &
                     dimension(:), &
                     allocatable, target:: subgrid_array, subbuffer_array
    real(kind = wp), asynchronous, &
                     dimension(:), &
                     allocatable, target:: testgrid_array, testbuffer_array
        real(kind = wp), pointer, &
                         dimension(:,:,:):: subgrid_pointer => null(),   &
                                            subbuffer_pointer => null(), &
                                            testgrid_pointer => null(),  &
                                            testbuffer_pointer => null()


    ! Input Grid Data
    INTEGER:: i, m, num_val, arg_num
    CHARACTER(len = 32):: arg

    ! MPI Parameters
    INTEGER:: world_size, my_rank, my_row, my_col
    INTEGER, DIMENSION(2):: dims_tasks_2d, coords
    INTEGER, DIMENSION(3):: ar_shape
    LOGICAL, DIMENSION(2):: periods = [.false., .false.]
    TYPE(MPI_Comm):: MPI_COMM_CART, comm_myRow, comm_myColumn
    INTEGER, DIMENSION(:), allocatable:: rcounts, disp
    
    ! Formating:
    character(len = 100):: fmt

    ! Error Integer
    INTEGER, dimension(100):: ierr  = 0

    ! Body======================================================================
    ! Initialisation------------------------------------------------------------

    ! MPI Initialisation
    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    call MPI_Comm_size(MPI_COMM_WORLD, world_size)

     ! Read Input Parameters from Terminal---------------------------------------
     if (my_rank == 0) then
        do i = 1, COMMAND_ARGUMENT_COUNT()  ! Should be 3
            ! Get Arguments:
            call getarg(i, arg)
                READ(arg, '(I10)') subgrid_xyz_dims(i)
            end do
     end if

    ! Broadcast Input Parameters and use pointers for better readability
    call MPI_BCAST(subgrid_xyz_dims, 3, MPI_INTEGER, 0, MPI_COMM_WORLD)
    ! Defines the number of tasks in 2d grid
    call get_task_dims(world_size, subgrid_xyz_dims(2), dims_tasks_2d)

    ! Create Communicator-------------------------------------------------------
    call MPI_CART_CREATE(MPI_COMM_WORLD, 2, dims_tasks_2d, periods, .true., MPI_COMM_CART, ierr(1)) 
    if (ierr(1)/= 0) error stop "Comm Cart Not Valid"

    call MPI_Comm_rank(MPI_COMM_CART, my_rank)
    call MPI_CART_COORDS(MPI_COMM_CART, my_rank, 2, coords, ierr(1))
    ! call MPI_Comm_split(MPI_COMM_CART, coords(1), coords(2), comm_myColumn, ierr(1))
    ! call MPI_Comm_split(MPI_COMM_CART, coords(2), coords(1), comm_myRow, ierr(1))

    ! Grid Initiation-----------------------------------------------------------
    state = 2
    ! Initiate SubGrid
    call subgrid_handler%init(state, subgrid_xyz_dims)
    call subgrid_handler%allocate_arrays(state, subgrid_array, subbuffer_array)
    subgrid_pointer = subgrid_handler%grid_pointer
    subbuffer_pointer = subgrid_handler%buffer_pointer

    ! Initiate Test Grid
    call testgrid_handler%init(state, [subgrid_xyz_dims(1)*dims_tasks_2d(1), &                                    
                                       subgrid_xyz_dims(2), &
                                       subgrid_xyz_dims(3)*dims_tasks_2d(2)])
    call testgrid_handler%allocate_arrays(state, testgrid_array, testbuffer_array)
    testgrid_pointer   = testgrid_handler%grid_pointer
    testbuffer_pointer = testgrid_handler%buffer_pointer

    ! Set Values For Grids------------------------------------------------------
    subgrid_array = my_rank
    testgrid_array =  99

    ! Send Subgrids------------------------------------------------------------
    ! rcounts
    allocate(rcounts(world_size), stat = ierr(3))

    ! disp
    allocate(disp(world_size), stat = ierr(4))

    send_num = prod(subgrid_handler%subgrid_xyz_dims)
    do i = 1, world_size, 1
        rcounts(i) = send_num
        disp(i) = (i-1)*send_num
    end do

    ! !write(*,*) "Before Barrier", my_rank
    ! !call MPI_BARRIER(MPI_COMM_WORLD)

    call MPI_Gatherv(sendbuf = subgrid_array, &
                     sendcount = send_num, &
                     sendtype = MPI_REAL, &
                     recvbuf = testbuffer_array, &
                     recvcounts = rcounts, &
                     displs = disp, &
                     recvtype = MPI_REAL, &
                     root = 0, &
                     comm = MPI_COMM_CART, &
                     ierror = ierr(1))

    ! Write For Testing
     if (my_rank == 0) then 
        ! write(*,*) subgrid_handler%grid_pointer  
        ! write(fmt, '(A, I0, A)') '(', subgrid_handler%subgrid_xyz_dims(1), 'I4)'
        write(fmt, '(A, I0, A)') '(', subgrid_handler%subgrid_xyz_dims(1), 'F4.0)'
        write(*,fmt) subgrid_handler%grid_pointer  
        ! call print_cube_views(subgrid_handler%grid_pointer, subgrid_xyz_dims(1), &
        !                                                     subgrid_xyz_dims(2), &
        !                                                     subgrid_xyz_dims(3))
        write(*,*) "my grid rank 0"
        write(fmt, '(A, I0, A)') '(', testgrid_handler%subgrid_xyz_dims(1), 'F4.0)'
        write(*,fmt) testgrid_handler%buffer_pointer
        ! ar_shape = shape(test_grid)
     end if
     ! write(*,*)


    ! !call MPI_BARRIER(MPI_COMM_WORLD)
    ! !write(*,*) "After cube view", my_rank

    ! if (my_rank == 0) then
    !     call print_cube_views(test_grid, sub_grid_y(1), sub_grid_y(2), sub_grid_y(3)*2)
    !     write(*,*) "Cube views Rank 0"
    ! end if

    ! !write(*,*) "After Send Barrier", my_rank
    ! !call MPI_BARRIER(MPI_COMM_WORLD)






    ! ! Cleanup-------------------------------------------------------------------
    ! ierr(1) = 0
    ! if (allocated(my_grid_al)) then
    !     deallocate(my_grid, stat = ierr(1))l50151
    ! end if
    ! if (ierr(1) /= 0) print *, "u(sub_grid_y), : Deallocation request denied"
    

    ! call MPI_Comm_free(comm_myRow, ierr(1))
    ! call MPI_Comm_free(comm_myColumn, ierr(1))
    
    call MPI_BARRIER(MPI_COMM_CART)
    ! write(*,*) "After Barrier", my_rank, coords
    call MPI_Comm_free(MPI_COMM_CART, ierr(1))
    call MPI_Finalize()


end program comm_test
