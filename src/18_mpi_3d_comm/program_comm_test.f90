program comm_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use mpi_f08
    use grid_utils
    use grid_handler
    use compgrid_handler
    ! use MATRIX_OUTPUT
    implicit none 

    ! Parameters================================================================
    ! Grid definition, sub_grid_y = [x_cy*, y_s, z_cy*]
    ! INTEGER, DIMENSION(3), target:: sub_grid_y

    integer                                :: send_num
    integer, dimension(3)                  :: state_xyz
    type(Grid)                             :: subgrid_handler, subgrid_handler2
    type(Complete_Grid)                    :: testgrid_handler
    integer, dimension(3)                  :: subgrid_xyz_dims, grid_xyz_dims, dims
    integer, dimension(2)                  :: task_state
    real(kind = wp), asynchronous, &
                     dimension(:), &
                     allocatable, target   :: subgrid_array, subgrid_array2!, subbuffer_array
    real(kind = wp), asynchronous, &
                     dimension(:), &
                     allocatable, target   :: testgrid_array, testbuffer_array
        real(kind = wp), pointer, &
                         dimension(:,:,:)  :: subgrid_pointer => null(),   &
                                             ! subbuffer_pointer => null(), &
                                             testgrid_pointer => null(),  &
                                             testbuffer_pointer => null(), &
                                             temp_pointer => null()
                                            


    ! Input Grid Data
    INTEGER:: i, j, k, m, p, num_val, arg_num
    CHARACTER(len = 32):: arg

    ! MPI Parameters
    INTEGER:: world_size, my_rank, my_row, my_col
    INTEGER, DIMENSION(2):: dims_tasks_2d, coords
    INTEGER, DIMENSION(3):: ar_shape
    LOGICAL, DIMENSION(2):: periods = [.false., .false.]
    TYPE(MPI_Comm):: MPI_COMM_CART, MPI_COMM_Row, MPI_COMM_Column 
    INTEGER, DIMENSION(:), allocatable:: rcounts, disp
    character(len = 200):: mpi_out
    
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

    ! Define Complete Grid------------------------------------------------------
    ! Broadcast Input Parameters and use pointers for better readability
    call MPI_BCAST(subgrid_xyz_dims, 3, MPI_INTEGER, 0, MPI_COMM_WORLD)
    ! Defines the number of tasks in 2d grid
    state_xyz  = [2, 1, 3]
    task_state = [2, 1] ! If you use MPI_COMM_CART, use [2, 1]
    call get_task_dims(world_size, subgrid_xyz_dims(state_xyz(1)), dims_tasks_2d)

    ! Create Communicator-------------------------------------------------------
    call MPI_CART_CREATE(MPI_COMM_WORLD, 2, dims_tasks_2d, periods, .true., MPI_COMM_CART, ierr(1)) 
    if (ierr(1)/= 0) error stop "Comm Cart Not Valid"

    call MPI_Comm_rank(MPI_COMM_CART, my_rank)
    call MPI_CART_COORDS(MPI_COMM_CART, my_rank, 2, coords, ierr(1))
    ! Grid Initiation-----------------------------------------------------------

    ! Initiate SubGrid
    call subgrid_handler%init(state_xyz, subgrid_xyz_dims)
    call subgrid_handler%allocate_arrays(subgrid_array)

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

    ! Set Values For Grids------------------------------------------------------
    subgrid_array = my_rank
    if (my_rank == 0) then
        do i = 1, size(subgrid_handler%grid_pointer_1d), 1
            subgrid_handler%grid_pointer_1d(i) = 90+i
        end do
    end if
    testbuffer_array = 99
    testgrid_array =  99

    ! Send Subgrids to bigGrid--------------------------------------------------

    call subgrid_handler2%init(state_xyz, subgrid_xyz_dims)
    call subgrid_handler2%allocate_arrays(subgrid_array2)
    subgrid_array2 = subgrid_array
    call subgrid_handler%get_dims(dims)
    ! call dns_transpose(subgrid_handler%grid_pointer_3d(1, :,:), &
    !                    dims(2), dims(3), dims(3), &
    !                    subgrid_handler2%grid_pointer_3d(1, :,:), & 
    !                    dims(3))
    ! rcounts
    allocate(rcounts(world_size), stat = ierr(3))

    ! disp
    allocate(disp(world_size), stat = ierr(4))

    send_num = prod(subgrid_handler%grid_xyz_dims)
    do i = 1, world_size, 1
        rcounts(i) = send_num
        disp(i) = (i-1)*send_num
    end do

    call MPI_Gatherv(sendbuf = subgrid_handler2%grid_pointer_1d, &
                     sendcount = send_num, &
                     sendtype = MPI_DOUBLE, &
                     recvbuf = testgrid_handler%buffer_pointer_1d, &
                     recvcounts = rcounts, &
                     displs = disp, &
                     recvtype = MPI_DOUBLE, &
                     root = 0, &
                     comm = MPI_COMM_CART, &
                     ierror = ierr(1))


    ! Write For Testing
     if (my_rank == 0) then 
        call testgrid_handler%reorder_gatherv()!, dims_tasks_2d, subgrid_xyz_dims)
     end if

    ierr = 0
    if (allocated(rcounts)) deallocate(rcounts, stat = ierr(1))
    if (allocated(disp)) deallocate(disp, stat = ierr(2))
    if (sum(ierr) /= 0) print *, "u(sub_grid_y), : Deallocation request denied"

    ! SubComm Initialization----------------------------------------------------
    ! call MPI_Comm_split(MPI_COMM_CART, coords(1), coords(2), MPI_COMM_Column, ierr(1))
    call MPI_Comm_split(MPI_COMM_CART, coords(2), coords(1), MPI_COMM_Row, ierr(1))
    call MPI_Comm_rank(MPI_COMM_Row, my_col)
    call MPI_Comm_size(MPI_COMM_Row, world_size)

    ! Subgrid SubgridComm-------------------------------------------------------



    ! subroutine dns_transpose(a, nra, nca, ma, b, mb)
    !     use TLAB_CONSTANTS
    !     implicit none

    !     integer(wi), intent(in):: nra      ! Number of rows in a
    !     integer(wi), intent(in):: nca      ! Number of columns in b
    !     integer(wi), intent(in):: ma       ! Leading dimension on the input matrix a
    !     integer(wi), intent(in):: mb       ! Leading dimension on the output matrix b
    !     real(wp), intent(in)    :: a(ma, *)  ! Input array
    !     real(wp), intent(out)   :: b(mb, *)  ! Transposed array
    
    if (my_rank == 0) write(*,*)  "Before switch"
    if (my_rank == 0) write(*,*)  subgrid_handler%complete_pointer_1d
    call subgrid_handler%switch_dims_12()
    if (my_rank == 0) write(*,*)  "After switch"
    if (my_rank == 0) write(*,*)  subgrid_handler%complete_pointer_1d


    ! rcounts
    allocate(rcounts(world_size), stat = ierr(3))

    ! disp
    allocate(disp(world_size), stat = ierr(4))

    send_num =  prod(subgrid_handler%grid_xyz_dims)/world_size

    ! call MPI_Gather(sendbuf    = subgrid_handler%grid_pointer_1d, &
    !                 sendcount  = send_num, &
    !                 sendtype   = MPI_DOUBLE, &
    !                 recvbuf    = subgrid_handler2%grid_pointer_1d, &
    !                 recvcount  = send_num, &
    !                 recvtype   = MPI_DOUBLE, &
    !                 root       = 0, &
    !                 comm       = MPI_COMM_Row, &
    !                 ierror     = ierr(1))

    ! call subgrid_handler2%reorder_gatherv_sub(world_size, subgrid_handler%grid_pointer_3d)

    if (my_rank == 0) then
        write(*,*) ""
        write(*,*) ""
        write(*,*) ""
        call subgrid_handler2%get_dims(dims)
        write(*,*) "Dims (1, :,:)"
        write(fmt, '(A, I0, A)') '(', dims(2), 'F4.0)'
        write(*,fmt) subgrid_handler2%grid_pointer_3d(1, :,:)

        write(*,*) "Dims (:,1, :)"
        write(fmt, '(A, I0, A)') '(', dims(1), 'F4.0)'
        write(*,fmt) subgrid_handler2%grid_pointer_3d(:, 1, :)

        write(*,*) "Dims (:,:,1)"
        write(fmt, '(A, I0, A)') '(', dims(1), 'F4.0)'
        write(*,fmt) subgrid_handler2%grid_pointer_3d(:, :, 1)

        write(*,*) subgrid_handler2%grid_pointer_3d
    end if

    ! call MPI_BARRIER(MPI_COMM_CART)
    ! write(mpi_out, *) "My Rank: ", my_rank, ", My Coords: ", coords, ", Tasks m and n: ", dims_tasks_2d
    ! call gather_and_print_characters(mpi_out, MPI_COMM_WORLD)



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
