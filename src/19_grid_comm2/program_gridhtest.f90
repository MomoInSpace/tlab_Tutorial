program comm_test
    use mpi_f08
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use TLAB_POINTERS_3D
    ! use grid_utils
    use grid_handler
    use grid_comm_module
    use grid_debug
    ! use compgrid_handler
    ! use MATRIX_OUTPUT
    implicit none 

    ! Parameters================================================================
    ! Grid definition, sub_grid_y = [x_cy*, y_s, z_cy*]
    ! INTEGER, DIMENSION(3), target:: sub_grid_y

    ! Input Grid Data
    INTEGER:: i 
    CHARACTER(len = 32):: arg

    integer:: world_size, my_rank
    type(Grid3D_Comm_Handler):: grid_comm_handler
    type(Grid3D_cpu):: grid_handler
    type(Complete_grid_debugger):: testgrid_handler
    integer, dimension(3)                  :: state_xyz
    integer, dimension(3)                  :: subgrid_xyz_dims, grid_xyz_dims, dims
    integer, dimension(2)                  :: task_state

    ! Error Integer
    INTEGER, dimension(100):: ierr  = 0

    ! Debug arrays
    real(kind = wp), asynchronous, &
                     dimension(:), &
                     allocatable, target   :: testgrid_array, testbuffer_array
    INTEGER, DIMENSION(2):: dims_tasks_2d
    integer                                :: send_num

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

    ! Define Complete Grid------------------------------------------------------
    ! Broadcast Input Parameters and use pointers for better readability
    call MPI_BCAST(subgrid_xyz_dims, 3, MPI_INTEGER, 0, MPI_COMM_WORLD)
    ! Defines the number of tasks in 2d grid
    state_xyz  = [2, 1, 3]
    task_state = [2, 1] ! If you use MPI_COMM_CART, use [2, 1]

    ! Allocate Arrays:

    ! Initiate derived types
    call grid_handler%init(state_xyz, subgrid_xyz_dims, 2)
    allocate(q(grid_handler%total_space, 1), stat = ierr(1))
    if (ierr(1) /= 0) print *, "q(1, grid_handler%total_space), : Allocation request denied"
    q = my_rank
    call grid_handler%set_pointer_1D(q(:, 1))
    call grid_handler%get_pointer_3D(u)

    call grid_comm_handler%init(world_size, subgrid_xyz_dims(state_xyz(1)))
    dims_tasks_2d = grid_comm_handler%MPI_CART_DIMS
    ! Cleanup ==================================================================
    if (allocated(q )) deallocate(q, stat = ierr(1))
    if (ierr(1) /= 0) print *, "q(1, grid_handler%total_space), : Deallocation request denied"
                    
    ! Initiate Test Grid
    ! First we initialize the total grid as subgrid
    grid_xyz_dims = [subgrid_xyz_dims(1), &                                    
                     subgrid_xyz_dims(2), &
                     subgrid_xyz_dims(3)]
    ! Now we multiply the last two dimensions of the grid with 
    ! the dimensions of the tasks in said direction
    grid_xyz_dims(state_xyz(2)) = grid_xyz_dims(state_xyz(2))*dims_tasks_2d(task_state(1))
    grid_xyz_dims(state_xyz(3)) = grid_xyz_dims(state_xyz(3))*dims_tasks_2d(task_state(2))

    call testgrid_handler%init_complete(state_xyz, grid_xyz_dims, &
                                        subgrid_xyz_dims, &
                                        dims_tasks_2d, &
                                        task_state)
    call testgrid_handler%allocate_arrays_wbuffer(testgrid_array, testbuffer_array)

    send_num = prod(grid_handler%grid_xyz_dims)
    call MPI_Gather(sendbuf    = grid_handler%grid_space, &
                    sendcount  = send_num, &
                    sendtype   = MPI_DOUBLE, &
                    recvbuf    = testgrid_handler%buffer_pointer_1d, &
                    recvcount  = send_num, &
                    recvtype   = MPI_DOUBLE, &
                    root       = 0, &
                    comm       = grid_comm_handler%MPI_COMM_CART, &
                    ierror     = ierr(1))

    ! Write For Testing
     if (my_rank == 0) then 
        call testgrid_handler%reorder_gatherv()!, dims_tasks_2d, subgrid_xyz_dims)
     end if

end program comm_test
