program comm_test
    use mpi_f08
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use TLAB_POINTERS_3D
    use grid_handler
    use grid_comm_module
    use grid_debug
    implicit none 

    ! Parameters================================================================
    ! Input Grid Data
    INTEGER:: i 
    CHARACTER(len = 32):: arg

    integer:: world_size, my_rank
    type(Grid3D_Comm_Handler):: grid_comm_handler
    type(Grid3D_cpu):: grid_handler, grid_handler_rcv
    type(Complete_grid_debugger):: testgrid_handler
    integer, dimension(3)                  :: state_xyz
    integer, dimension(3)                  :: subgrid_xyz_dims
    integer, dimension(2)                  :: task_state

    ! Error Integer
    INTEGER, dimension(100):: ierr  = 0

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

    ! Define The Initial Sate of the grid--------------------------------------
    state_xyz  = [2, 1, 3]

    !Init grid_comm_handler-----------------------------------------------------
    call grid_comm_handler%init(world_size, subgrid_xyz_dims(state_xyz(1)))

    ! Init grid_handler derived types-------------------------------------------
    call grid_handler%init(state_xyz, subgrid_xyz_dims, 2, grid_comm_handler%MPI_Cart_Dims)
    call grid_handler_rcv%init(state_xyz, subgrid_xyz_dims, 2, grid_comm_handler%MPI_Cart_Dims)

    allocate(x(grid_handler%total_space, 1), stat = ierr(1))
    if (ierr(1) /= 0) print *, "q(1, grid_handler%total_space), : Allocation request denied"
    allocate(q(grid_handler%total_space, 1), stat = ierr(1))
    if (ierr(1) /= 0) print *, "q(1, grid_handler%total_space), : Allocation request denied"

    call grid_handler%set_pointer_1D(x(:,1))
    call grid_handler_rcv%set_pointer_1D(q(:,1))

    ! Example: For later use
    call grid_handler%get_pointer_3D(u)


    ! Set Values For visualization----------------------------------------------
    grid_handler%grid_space = my_rank
    grid_handler_rcv%grid_space = 99

    if (my_rank == 0 ) then
        do i = 1, size(grid_handler%grid_space)
            grid_handler%grid_space(i) = i
        end do
    end if 

    ! if (my_rank == 0) write(*,*) x

    ! Visualize Complete Grid--------------------------------------------------

    call gather_compgrid(grid_handler, grid_comm_handler, &
                         subgrid_xyz_dims, &
                         testgrid_handler, my_rank)

    ! Rotation 1------------------------------------------------------=========
    call grid_comm_handler%rotate_grid_row_213_cpu(grid_handler, grid_handler_rcv, .true.)

    ! Visualize Complete Grid--------------------------------------------------
    call gather_compgrid(grid_handler_rcv, grid_comm_handler, &
                         subgrid_xyz_dims, &
                         testgrid_handler, my_rank)

    ! Rotation 2------------------------------------------------------=========
    call grid_comm_handler%rotate_grid_row_213_cpu(grid_handler_rcv, grid_handler, .false.)


    ! Visualize Complete Grid--------------------------------------------------
    call gather_compgrid(grid_handler, grid_comm_handler, &
                         subgrid_xyz_dims, &
                         testgrid_handler, my_rank)


    ! Cleanup ==================================================================
    if (allocated(q )) deallocate(q, stat = ierr(1))
    if (ierr(1) /= 0) print *, "q(1, grid_handler%total_space), : Deallocation request denied"

    if (allocated(x )) deallocate(x, stat = ierr(1))
    if (ierr(1) /= 0) print *, "q(1, grid_handler%total_space), : Deallocation request denied"

    call MPI_Comm_free(grid_comm_handler%MPI_COMM_CART, ierr(1))
    call MPI_Comm_free(grid_comm_handler%MPI_COMM_Row, ierr(1))
    call MPI_Comm_free(grid_comm_handler%MPI_COMM_Column, ierr(1))

end program comm_test
