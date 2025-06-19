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

    integer                     :: world_size, my_rank 
    type(Grid3D_Comm_Handler)   :: grid_comm_handler
    type(Grid3D_cpu), target    :: grid_handler_A, grid_handler_B, grid_handler_tmp
    type(Complete_grid_debugger):: testgrid_handler
    integer, dimension(3)       :: state_xyz  = [2, 1, 3], &
                                   ! Pertubation of [1, 2, 3]. 
                                   ! Describes the orientation of our array/grid. 
                                   block_xyz_dims, &
                                   ! Describes the size of the smallest grid_unit.
                                   block_multiplication_xyz_state = [12, 1, 2], &
                                   ! The block_xyz_dims get multiplied by the 
                                   ! threads as indicated by this state.
                                   ! Depending on the communication algorithm used, 
                                   ! the programm needs different states!
                                   subgrid_xyz_dims
                                   ! The size of the subgrid for each thread as 
                                   ! indicated by block_multiplication_xyz_state
    integer                     :: column_upper_limit
    integer, dimension(2)       :: task_state

    ! Error Integer
    INTEGER, dimension(100)     :: ierr  = 0

    integer                     :: send_num, overhead_factor

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
            READ(arg, '(I10)') block_xyz_dims(i)
            end do
     end if

    ! Define Complete Grid------------------------------------------------------
    ! Broadcast Input Parameters and use pointers for better readability
    call MPI_BCAST(block_xyz_dims, 3, MPI_INTEGER, 0, MPI_COMM_WORLD)
    column_upper_limit = block_xyz_dims(2)

    !Init grid_comm_handler-----------------------------------------------------
    ! Four possible states: [0, 1, 2, 12].
    call grid_comm_handler%init(world_size, &
            block_xyz_dims, &
            block_multiplication_xyz_state, &
            column_upper_limit)
    subgrid_xyz_dims = grid_comm_handler%subgrid_xyz_dims  

    ! Init grid_handler derived types-------------------------------------------
    overhead_factor = 3
    call grid_handler_A%init(state_xyz, subgrid_xyz_dims, overhead_factor, grid_comm_handler%MPI_Cart_Dims)
    call grid_handler_B%init(state_xyz, subgrid_xyz_dims, overhead_factor, grid_comm_handler%MPI_Cart_Dims)
    call grid_handler_tmp%init(state_xyz, subgrid_xyz_dims, overhead_factor, grid_comm_handler%MPI_Cart_Dims)

    allocate(x(grid_handler_A%total_space, 3), stat = ierr(1))
    if (ierr(1) /= 0) print *, "q(1, grid_handler%total_space), : Allocation request denied"
    ! allocate(q(grid_handler%total_space, 1), stat = ierr(1))
    ! if (ierr(1) /= 0) print *, "q(1, grid_handler%total_space), : Allocation request denied"

    call grid_handler_A%set_pointer_1D(x(:,1))
    call grid_handler_B%set_pointer_1D(x(:,2))
    call grid_handler_tmp%set_pointer_1D(x(:,3))

    ! Example: For later use
    call grid_handler_A%get_pointer_3D(u)


    ! Set Values For visualization----------------------------------------------
    grid_handler_A%grid_space = my_rank + 1
    grid_handler_B%grid_space = 99

    if (my_rank == 0 ) then
        do i = 1, size(grid_handler_A%grid_space)
            grid_handler_A%grid_space(i) = i-1
        end do
    end if 

    call grid_handler_A%get_pointer_3D(u)
    ! if (my_rank == 0) write(*,*) x

    ! Visualize Complete Grid---------------------------------------------------
    call debug_values(grid_handler_A)

    ! Rotation 1----------------------------------------------------------------
    call grid_comm_handler%rotate_grid_cpu(grid_handler_A, grid_handler_B, [2,1,3], grid_handler_tmp)
    call grid_handler_B%grid_waitall()
    !call grid_handler_B%get_pointer_3D(u)

    ! Visualize Complete Grid--------------------------------------------------
    call debug_values(grid_handler_B)

    ! Rotation 2---------------------------------------------------------------
    call grid_comm_handler%rotate_grid_cpu(grid_handler_B, grid_handler_A, [3,2,1],grid_handler_tmp)
    call grid_handler_A%grid_waitall()

    ! Visualize Complete Grid--------------------------------------------------
    call debug_values(grid_handler_A)

    ! Rotation 3---------------------------------------------------------------
    call grid_comm_handler%rotate_grid_cpu(grid_handler_A, grid_handler_B, [3,2,1], grid_handler_tmp)
    call grid_handler_B%grid_waitall()

    call MPI_Barrier(MPI_COMM_WORLD)

    ! ! Rotation 4---------------------------------------------------------------
    call grid_comm_handler%rotate_grid_cpu(grid_handler_B, grid_handler_A, [2,1,3],grid_handler_tmp)
    call grid_handler_A%grid_waitall()

    ! ! Visualize Complete Grid--------------------------------------------------
    call debug_values(grid_handler_A)

    ! Cleanup ==================================================================
    if (allocated(x )) deallocate(x, stat = ierr(1))
    if (ierr(1) /= 0) print *, "q(1, grid_handler_A%total_space), : Deallocation request denied"

     if (allocated(q )) deallocate(q, stat = ierr(1))
     if (ierr(1) /= 0) print *, "q(1, grid_handler_A%total_space), : Deallocation request denied"

    ! Allocate testgrid and testbuffer . . . . . . . . . . . . . . . . . . . . 
    if (allocated(testgrid_array)) deallocate(testgrid_array, stat = ierr(1))
    if (ierr(1) /= 0) print *, "array: Deallocation request denied"

    if (allocated(testbuffer_array)) deallocate(testbuffer_array, stat = ierr(1))
    if (ierr(1) /= 0) print *, "array: Deallocation request denied"

    ! Calling 
    call grid_comm_handler%free()

    contains

    subroutine debug_values(current_grid_handler)
        type(Grid3D_cpu)            :: current_grid_handler
        call testgrid_handler%gather_compgrid(current_grid_handler, grid_comm_handler, subgrid_xyz_dims, my_rank)
        call testgrid_handler%visualize_grid(my_rank)
        !call calc_checksum(my_rank)
    end subroutine debug_values


end program comm_test

