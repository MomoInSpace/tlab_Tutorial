module grid_comm_module
    use mpi_f08
    use grid_handler
    implicit none

    private:: get_factors
    private:: get_task_dims

    type:: Grid3D_Comm_Handler
        ! Grid3D_Communicator  handles the communication of the 3D grid.

        TYPE(MPI_Comm):: MPI_Comm_Row, MPI_Comm_Column, MPI_COMM_CART
        integer              :: world_size, row_size, column_size
        integer              :: cart_rank, row_rank, column_rank
        integer, dimension(2):: MPI_Cart_Dims, MPI_Cart_Coords

    contains
        procedure:: init
        procedure:: rotate_grid_row_213_cpu
        ! procedure:: rotate_grid_col_312

    end type Grid3D_Comm_Handler


contains

    subroutine init(self, world_size, vertical_dimension, subgrid_factors)
        class(Grid3D_Comm_Handler):: self
        integer              :: world_size, rank, vertical_dimension
        integer, dimension(2):: task_dims  
        logical, dimension(2):: periods = [.false., .false.]
        ! Error integer:
        integer, dimension(100):: ierr  = 0
        integer, dimension(3), intent(out):: subgrid_factors

        call get_task_dims(world_size, vertical_dimension, self%MPI_Cart_Dims)

        call MPI_CART_CREATE(MPI_COMM_WORLD, 2, self%MPI_Cart_Dims, periods, .true., self%MPI_COMM_CART, ierr(1)) 

        call MPI_Comm_rank(self%MPI_COMM_CART, self%cart_rank)
        call MPI_CART_COORDS(self%MPI_COMM_CART, self%cart_rank, 2, self%MPI_Cart_Coords, ierr(2))

        self%row_size = self%MPI_Cart_Dims(2)
        self%row_rank = self%MPI_Cart_Coords(2)
        self%column_size = self%MPI_Cart_Dims(1)
        self%column_rank = self%MPI_Cart_Coords(1)
        ! Create Row communicator
        call MPI_Comm_split(comm  = self%MPI_COMM_CART, &
                            color = self%column_rank, &
                            key   = self%row_rank, &
                            newcomm = self%MPI_Comm_Row, &
                            ierror = ierr(3))

        ! Create Column communicator
        call MPI_Comm_split(comm  = self%MPI_COMM_CART, &
                            color = self%row_rank, &
                            key   = self%column_rank, &
                            newcomm = self%MPI_Comm_Column, &
                            ierror   = ierr(4))

        subgrid_factors = [self%column_size, 1, 1]
        ! subgrid_factors = [1, 1, 1]
        
    end subroutine init

    subroutine rotate_grid_row_213_cpu(self, grid_handler_send, grid_handler_rcv, forward)
        class(Grid3D_Comm_Handler)         :: self
        class(Grid3D_cpu), intent(inout)   :: grid_handler_send, grid_handler_rcv
        real(kind = wp), pointer, &
                         dimension(:,:,:):: work_space_send, grid3D_pointer_send, grid3D_pointer_rcv
        integer, dimension(3)            :: dims_send, dims_rcv
        integer                          :: send_count, surface_area, ierr0, i, j, k, m, n, my_rank
        integer, dimension(:), &
                 allocatable             :: ierr, root, rcv_j
        real(kind = wp), pointer, dimension(:):: send_buf_pointer
        logical:: forward


        call MPI_Comm_rank(self%MPI_COMM_CART, my_rank)
        call grid_handler_send%get_switch_dims_213_workspace(dims_send, work_space_send, grid3D_pointer_send)
        

        allocate(ierr(dims_send(3)*self%row_size), stat = ierr0)
        if (ierr0 /= 0) print *, "ierr(dim(3)): Allocation request denied"
        ierr = 0

        call grid_handler_rcv%perturb_state(state_xyz            = grid_handler_send%state_xyz, &
                                            grid_xyz_dims        = grid_handler_send%grid_xyz_dims, &
                                            subgrid_factors_123  = [1, self%row_size, 1], &
                                            subgrid_dividers_123 = [self%row_size, 1, 1], &
                                            perturbation_123      = [2, 1, 3]) 
        call grid_handler_rcv%get_pointer_3D(grid3D_pointer_rcv)
        dims_rcv = grid_handler_rcv%get_dims()

        ! if (my_rank == 0) write(*,*) "Dims Send:", dims_send
        ! if (my_rank == 0) write(*,*) 'Dims Rcv: ', dims_rcv


        ! surface_area = dims(1)*dims(2)
        send_count = dims_send(2)  ! surface_area/self%row_size

        j = dims_send(1)/self%row_size
        k = 0

        allocate(root(dims_send(1)), stat = ierr0)
        allocate(rcv_j(dims_send(1)), stat = ierr0)

        do i = 1, dims_send(1), j
            root(i:i+j-1) = k
            k = k+1
            n = 1
            do m = 1, j
               rcv_j(i+m-1) = n
                n = n+1
            end do
        end do



        ! if (my_rank == 0) write(*,*) 'Rotation 1', "Step", m, 'Rank', my_rank, 'grid_space:', grid3D_pointer_rcv
        do k = 1, dims_send(3) 
            do j = 1, dims_send(1) 
                do i = 1, dims_send(2) 
                    work_space_send(i, j, k) =  grid3D_pointer_send(j, i, k)
                end do
            end do

            do j = 1, dims_send(1) 
                ! Each process A, B, C has to scatter their data to all other processes in its row:
                ! Look at one (1, 2)-surface:
                !       A     B     C 
                !    |1 1 1|2 2 2|3 3 3|    A |1 1 1 2 2 2 3 3 3|
                !    |     |     |     |      |-----------------|
                !    |1 1 1|2 2 2|3 3 3| -> B |1 1 1 2 2 2 3 3 3|
                !    |     |     |     |      |-----------------|
                !    |1 1 1|2 2 2|3 3 3|    C |1 1 1 2 2 2 3 3 3|

                call MPI_Gather(sendbuf   = work_space_send(:, j, k), &
                                sendcount  = send_count, &
                                sendtype   = MPI_DOUBLE, &
                                recvbuf    = grid3D_pointer_rcv(:,rcv_j(j), k), &
                                                ! (m*send_count+surface_area*(k-1)+1:), &
                                recvcount  = send_count, &
                                recvtype   = MPI_DOUBLE, &
                                root       = root(j), &
                                comm       = self%MPI_Comm_Row, &
                                ierror     = ierr0)
                                ! ierror     = ierr(m+(k-1)*m)
                                ! recvbuf    = work_space(1, 1, 1), &
                                ! recvbuf    = grid_handler_rcv%grid_space, &
                ! if (my_rank == 0) write(*,*) 'Rotation 1', "Step", m, 'Rank', my_rank, 'grid_space:', grid_handler_rcv%grid_space
                ! if (my_rank == 0 .and. root(j) == 0) write(*,*) 'Rotation 1', "Step", m, 'Rank', my_rank, 'grid_space:', grid3D_pointer_rcv
                ! if (my_rank == 0 .and. root(j) == 0) write(*,*) k, j, dims_send(3)
            end do


        end do


        ! if(my_rank == 0) write(*,*) "After Transformation:================="
        ! do i = 0, self%row_size*self%column_size-1
        !     call MPI_Barrier(MPI_COMM_WORLD)
        !         if (my_rank == i) write(*,*) 'Rotation 1', 'Rank', my_rank, 'grid_space:', grid3D_pointer_rcv, grid_handler_rcv%grid_xyz_dims
        !     end do
        ! call MPI_Barrier(MPI_COMM_WORLD)
        ! if(my_rank == 0) write(*,*) "=========================="

        if (sum(ierr) /= 0) error stop "Grid Row 213 Failed"

        if (allocated(ierr)) deallocate(ierr, stat = ierr0)
        if (ierr0 /= 0) print *, "ierr: Deallocation request denied, grid_row_213"

        if (allocated(root)) deallocate(root, stat = ierr0)
        if (ierr0 /= 0) print *, "ierr: Deallocation request denied, grid_row_213"

        ! call MPI_Barrier(MPI_COMM_WORLD)
        ! if (my_rank == 0) write(*,*) "Past Barrier"


    end subroutine rotate_grid_row_213_cpu

    subroutine get_task_dims(world_size, y_s, task_dims)
    ! Minimizes max(dims_task_2d), 
    !   while keeping in mind, that dims_task_2d(1) <= y_s
    !   and sum(task_dims) = world_size.
    !   For flat grids, this leads to task_dims(1) = y_s

    ! Parameters================================================================
    integer, intent(in):: world_size, y_s
    ! task_dims are the dimensions of the cartesian tasks 
    integer, dimension(2), intent(out):: task_dims  
    integer:: i, j, fac, m_num, n_num, factor_diff, my_rank
    integer, allocatable:: m_arr(:), n_arr(:)
    integer, allocatable:: factors_ys(:), factors_wsize(:)

    ! Body======================================================================
    ! Compute factors of y_s

    factors_ys = get_factors(y_s)
    factors_wsize = get_factors(world_size)

    n_num = world_size
    m_num = 1
    do i = 1, size(factors_ys)
        fac = factors_ys(i)
        if (any(factors_wsize == fac) .and. (fac+world_size/fac <= n_num+m_num) )then
            n_num = fac
            m_num = world_size/n_num
            end if
    end do

    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    if (my_rank == 0) then
        if (n_num /= y_s) print *, &
                "Note: n_num not divisible by y_s. For flat grids this reduces communication delay! "
        if (n_num /= y_s) print *, "n_num: ", n_num, ", y_s: ", y_s
    end if
    if (n_num == 1 .or. m_num == 1) error stop &
            "Fatal: world_size ist not divisible by facors of y_s, except 1, or calc error. Check code"
    if (n_num == 0) error stop &
            "Fatal: Something went terribly wrong. n_num in get_task_dims shouldn't be zero!!"

    task_dims = [m_num, n_num]

    end subroutine get_task_dims

    function get_factors(n) result(factors)
        integer, intent(in):: n
        integer, allocatable:: factors(:)
        integer:: i, count

        count = 0
        allocate(factors(0))
        do i = 1, n
            if (mod(n, i) == 0) then
                count = count+1
                factors = [factors, i]
            endif
        enddo
    end function get_factors



end module grid_comm_module
