program comm_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use mpi_f08
    use grid_utils
    ! use MATRIX_OUTPUT
    implicit none 

    ! Parameters================================================================
    ! Grid definition, sub_grid_y = [x_cy*, y_s, z_cy*]
    INTEGER, DIMENSION(3), target:: sub_grid_y
    integer:: x_cy_star, y_s, z_cy_star
    ! REAL(kind = wp), DIMENSION(:,:,:), ALLOCATABLE, ASYNCHRONOUS:: my_grid
    ! REAL(kind = wp), DIMENSION(:,:,:), ALLOCATABLE, ASYNCHRONOUS:: test_grid

    integer, pointer::  my_grid(:,:,:) => null(), &
                        test_grid(:,:,:) => null()
    integer, DIMENSION(:), ALLOCATABLE, ASYNCHRONOUS, Target:: my_grid_al
    integer, DIMENSION(:), ALLOCATABLE, ASYNCHRONOUS, Target:: test_grid_al
    INTEGER, DIMENSION(:), allocatable:: rcounts, disp

    ! Input Grid Data
    INTEGER:: i, m, num_val, arg_num
    CHARACTER(len = 32):: arg

    ! MPI Parameters
    INTEGER:: world_size, my_rank, my_row, my_col
    INTEGER, DIMENSION(2):: dims_tasks_2d, coords
    LOGICAL, DIMENSION(2):: periods = [.false., .false.]
    TYPE(MPI_Comm):: MPI_COMM_CART, comm_myRow, comm_myColumn
    

    ! Error Integer
    INTEGER, dimension(100):: ierr 

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
                 READ(arg, '(I10)') sub_grid_y(i)
             end do
     end if

     ! Broadcast Input Parameters and use pointers for better readability
     call MPI_BCAST(sub_grid_y, 3, MPI_INTEGER, 0, MPI_COMM_WORLD)
     x_cy_star = sub_grid_y(1)
     y_s       = sub_grid_y(2)
     z_cy_star = sub_grid_y(3)

    write(*,*) "Before Allocation Barrier", my_rank, sub_grid_y
    call MPI_BARRIER(MPI_COMM_WORLD)

    ! Allocation----------------------------------------------------------------
    ! Error Handling:
     ierr = 0
     ! my_grid:
     allocate(my_grid_al(&
              x_cy_star*y_s*z_cy_star), stat = ierr(1))
     my_grid(1:x_cy_star, 1:y_s, 1:z_cy_star) => my_grid_al  

     ! test_grid: only for test. Can be removes later, with the call
     call get_task_dims(world_size, sub_grid_y(2), dims_tasks_2d)
     if (dims_tasks_2d(1) <= 1 .or. dims_tasks_2d(2) <= 1) print *, &
         "Grid has Prime Decomposition. Don't use Primes as task numbers!"

     write(*,*) "My Task Dims", dims_tasks_2d, "of rank ", my_rank, "sub_grid:", sub_grid_y
     call MPI_BARRIER(MPI_COMM_WORLD)

     allocate(test_grid_al(prod(dims_tasks_2d)*prod(sub_grid_y)), stat = ierr(2))
     ! REMOVE 2!!!!!! ONLY for 1d test!!
     test_grid(1:dims_tasks_2d(1)*x_cy_star, &! REMOVE 2 !!!!!!!!!!!!!!!!!!!1
               1:y_s, &
               1:dims_tasks_2d(1)*dims_tasks_2d(2)*z_cy_star) => test_grid_al

     ! rcounts
     allocate(rcounts(world_size), stat = ierr(3))

     ! disp
     allocate(disp(world_size), stat = ierr(4))

     if (sum(ierr)/= 0) print *, "u(sub_grid_y), : Allocation request denied"

    ! Create Communicator-------------------------------------------------------
    !call get_task_dims(world_size, y_s, dims_tasks_2d) It test done, uncomment here.
    !!!!!!! TEST DIMS = 1, but you need to use 2!!!
    call MPI_CART_CREATE(MPI_COMM_WORLD, 2, dims_tasks_2d, periods, .true., MPI_COMM_CART, ierr(1)) 
    !if (ierr(1)/= 0) print *, "Comm Cart Not Valid"

    !call MPI_Comm_rank(MPI_COMM_CART, my_rank)
    !call MPI_CART_COORDS(MPI_COMM_CART, my_rank, 2, coords, ierr(1))
    ! call MPI_Comm_split(MPI_COMM_CART, coords(1), coords(2), comm_myColumn, ierr(1))
    ! call MPI_Comm_split(MPI_COMM_CART, coords(2), coords(1), comm_myRow, ierr(1))

    ! Send Subgrids------------------------------------------------------------
    ! Set values for testing
    ! my_grid = my_rank
    ! test_grid = 99

    ! do i = 1, world_size, 1
    !     rcounts(i) = prod(sub_grid_y)
    !     disp(i) = (i-1)*prod(sub_grid_y)
    ! end do
    
    !write(*,*) "Before Barrier", my_rank
    !call MPI_BARRIER(MPI_COMM_CART)

    !call MPI_Gatherv(sendbuf = my_grid_al, &
    !                 sendcount = prod(sub_grid_y), &
    !                 sendtype = MPI_INTEGER, &
    !                 recvbuf = test_grid_al, &
    !                 recvcounts = rcounts, &
    !                 displs = disp, &
    !                 recvtype = MPI_INTEGER, &
    !                 root = 0, &
    !                 comm = MPI_COMM_WORLD, &
    !                 ierror = ierr(1))

    ! Write For Testing
    ! if (my_rank == 0) then 
    !     !write(*,"(5I4)") my_grid
    !     write(*,*) "my grid rank 0"
    !     !write(*,"(5I4)") test_grid
    ! end if
    ! write(*,*)

    ! if (my_rank == 0) then
    !     !call print_cube_views(test_grid, sub_grid_y(1), sub_grid_y(2), sub_grid_y(3)*2)
    !     write(*,*) "Cube views Rank 0"
    ! end if

    ! write(*,*) "After Send Barrier", my_rank
    ! call MPI_BARRIER(MPI_COMM_WORLD)






    ! Cleanup-------------------------------------------------------------------
    !ierr(1) = 0
    !if (allocated(my_grid_al)) then
    !    deallocate(my_grid, stat = ierr(1))
    !end if
    !if (ierr(1) /= 0) print *, "u(sub_grid_y), : Deallocation request denied"
    

    ! call MPI_Comm_free(comm_myRow, ierr(1))
    ! call MPI_Comm_free(comm_myColumn, ierr(1))
    
    !write(*,*) "Before Barrier", my_rank
    !call MPI_BARRIER(MPI_COMM_CART)
    !call MPI_Comm_free(MPI_COMM_CART, ierr(1))
    call MPI_Finalize()


end program comm_test
