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
        integer, dimension(3):: complete_grid_xyz_dims, &
                                ! Dimensions of the complete grid
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
        ! grid_xyz_dims saves the length of each dimension x, y and z

    contains
        procedure:: init
        procedure:: rotate_grid_row_213_cpu
        procedure:: rotate_grid_col_321_cpu

    end type Grid3D_Comm_Handler


contains

    subroutine init(self, world_size,     &
                          block_xyz_dims, &
                          block_multiplication_xyz_state, &
                          column_upper_limit) 

        ! Parameters============================================================
        class(Grid3D_Comm_Handler):: self
        integer              :: world_size, rank, vertical_dimension, max_area
        integer, dimension(2):: task_dims  
        integer, dimension(3):: block_xyz_dims, &
                                ! Describes the size of the smallest grid_unit.
                                block_multiplication_xyz_state, &
                                ! The block_xyz_dims get multiplied by the 
                                ! threads as indicated by this state.
                                ! Depending on the communication algorithm used, 
                                ! the programm needs different states!
                                subgrid_xyz_dims
                                ! The size of the subgrid for each thread as 
                                ! indicated by block_multiplication_xyz_state
        integer              :: column_upper_limit
        logical, dimension(2):: periods = [.false., .false.]
        ! Error integer:
        integer, dimension(100):: ierr  = 0
        ! integer, dimension(3), intent(out):: subgrid_xyz_dims

        ! Body==================================================================
        self%block_xyz_dims = block_xyz_dims
        self%block_multiplication_xyz_state = block_multiplication_xyz_state

        ! Calculate Task Structure------------------------------------
        call get_task_dims(world_size, column_upper_limit, self%MPI_Cart_Dims)

        ! Create MPI Cart (Global) Communicator---------------------------------
        call MPI_CART_CREATE(MPI_COMM_WORLD, 2, self%MPI_Cart_Dims, periods, .true., self%MPI_COMM_CART, ierr(1)) 

        call MPI_Comm_rank(self%MPI_COMM_CART, self%cart_rank)
        call MPI_CART_COORDS(self%MPI_COMM_CART, self%cart_rank, 2, self%MPI_Cart_Coords, ierr(2))

        ! Define Row and Column Communicators----------------------------------
        ! Column size is always <= row size
        self%column_size = self%MPI_Cart_Dims(1)   
        self%column_rank = self%MPI_Cart_Coords(1)
        self%row_size = self%MPI_Cart_Dims(2)
        self%row_rank = self%MPI_Cart_Coords(2)
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

        ! Calculate Grid Structure------------------------------------
        call calculate_subgrid_dims(self)

    end subroutine init

               
    subroutine calculate_subgrid_dims(self)
        ! Parameters============================================================
        type(Grid3D_Comm_Handler):: self
        integer:: i
        ! Body =================================================================

        !write(*,*) self%block_xyz_dims
        do i=1, 3
            select case (self%block_multiplication_xyz_state(i))
            case(0)
                self%subgrid_xyz_dims(i) = self%block_xyz_dims(i)
            case(1)
                self%subgrid_xyz_dims(i) = self%block_xyz_dims(i)*self%column_size
            case(2) 
                self%subgrid_xyz_dims(i) = self%block_xyz_dims(i)*self%row_size
            case(12) 
                self%subgrid_xyz_dims(i) = &
                    self%block_xyz_dims(i)*self%row_size*self%column_size
            case default
                error stop "block_multiplication_xyz_state has invalid values"
            end select
        end do
        !write(*,*) self%subgrid_xyz_dims



    end subroutine

    ! TODO: Change to function
    subroutine get_task_dims(world_size, column_upper_limit, task_dims)
    ! Minimizes max(dims_task_2d), 
    !   while keeping in mind, that dims_task_2d(1) <= column_upper_limit
    !   and sum(task_dims) = world_size.
    !   For flat grids, this leads to task_dims(1) = column_upper_limit

    ! Parameters================================================================
    integer, intent(in):: world_size, column_upper_limit
    ! task_dims are the dimensions of the cartesian tasks 
    integer, dimension(2), intent(out):: task_dims  
    integer:: i, j, fac, m_num, n_num, factor_diff, my_rank
    integer, allocatable:: m_arr(:), n_arr(:)
    integer, allocatable:: factors_ys(:), factors_wsize(:)

    ! Body======================================================================
    ! Compute factors of column_upper_limit

    factors_ys = get_factors(column_upper_limit)
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
        if (n_num /= column_upper_limit) print *, &
                "Note: n_num not divisible by column_upper_limit. n_num <= column_upper_limit. Try to bring n_num and m_num as close to each other as possible. To reduce communication delay."
        if (n_num /= column_upper_limit) print *, "column_upper_limit: ", column_upper_limit, "n_num: ", n_num,  ', m_num', m_num
    end if
    
    if (n_num == 1 .or. m_num == 1) error stop &
            "Fatal: world_size ist not divisible by facors of column_upper_limit, except 1, or calc error. Check code"
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

    subroutine rotate_grid_row_213_cpu(self, grid_handler_send, grid_handler_rcv, overwrite)
        ! Parameters================================================================
        class(Grid3D_Comm_Handler)       :: self
        type(Grid3D_cpu), intent(inout)  :: grid_handler_send, &
                                            grid_handler_rcv
        logical                          :: overwrite
        integer                          :: send_count, &
                                            my_rank, &
                                            ierr0, i, j, k, m, n
        integer, dimension(3)            :: dims_send, &
                                            dims_rcv, &
                                            pertubation = [2,1,3], & 
                                            ! Pertubation describes how the state
                                            ! is changed after this subroutine
                                            ! is executed. [a,b,c] -> [c,b,a]
                                            subgrid_factors_xyz = [1, 1, 1], &
                                            subgrid_dividers_xyz = [1, 1, 1]
                                            ! subgrid_factors and subgrid_dividers
                                            ! describe how the subgrids are 
                                            ! squashed and stretched 
                                            ! in each xyz dimension
        integer, dimension(:), &
                 allocatable             :: ierr, &
                                            root, &
                                            rcv_j
        real(kind = wp), pointer, &
                         dimension(:)    :: send_buf_pointer, &
                                            work_space_send
        real(kind = wp), pointer, &
                         dimension(:,:,:):: work_space3D_send, &
                                            grid3D_pointer_send, &
                                            grid3D_pointer_rcv
        ! Notes=====================================================================
            ! Each process A, B, C has to scatter their data 
            !  to all other processes in its row:
            ! Look at one (1, 2)-surface:
            !       A     B     C 
            !    |1 1 1|2 2 2|3 3 3|    A |1 1 1 2 2 2 3 3 3|
            !    |     |     |     |      |-----------------|
            !    |1 1 1|2 2 2|3 3 3| -> B |1 1 1 2 2 2 3 3 3|
            !    |     |     |     |      |-----------------|
            !    |1 1 1|2 2 2|3 3 3|    C |1 1 1 2 2 2 3 3 3|
        ! Body======================================================================
        call MPI_Comm_rank(self%MPI_COMM_CART, my_rank)
        call grid_handler_send%get_switch_dims_workspace( &
                dims_send, &
                work_space3D_send, &
                work_space_send, &
                grid3D_pointer_send, &
                pertubation)
        
        subgrid_factors_xyz(pertubation(1))  = self%row_size
        subgrid_dividers_xyz(pertubation(2)) = self%row_size

        call grid_handler_rcv%perturb_state( &
                state_xyz            = grid_handler_send%state_xyz, &
                grid_xyz_dims        = grid_handler_send%grid_xyz_dims, &
                subgrid_factors_xyz  = subgrid_factors_xyz, &
                subgrid_dividers_xyz = subgrid_dividers_xyz, &
                perturbation_xyz     = pertubation) 

        call grid_handler_rcv%get_pointer_3D(grid3D_pointer_rcv)
        dims_rcv = grid_handler_rcv%get_dims()

        send_count = dims_send(pertubation(1))  ! surface_area/self%row_size

        ! Defining root and rcv_j and ierr for the communication later ---------
        allocate(root(dims_send(pertubation(2))), stat = ierr0)
        if (ierr0 /= 0) print *, "ierr(dim(3)): Allocation request denied"
        allocate(rcv_j(dims_send(pertubation(2))), stat = ierr0)
        if (ierr0 /= 0) print *, "ierr(dim(3)): Allocation request denied"
        allocate(ierr(dims_send(pertubation(3))*self%row_size))
        if (ierr0 /= 0) print *, "ierr(dim(3)): Allocation request denied"
        ierr = 0

        k = 0
        j = dims_send(pertubation(2))/self%row_size
        do i = 1, dims_send(pertubation(2)), j
            root(i:i+j-1) = k
            k = k+1
            n = 1
            do m = 1, j
               rcv_j(i+m-1) = n
                n = n+1
            end do
        end do

        ! Communication loop ---------------------------------------------------
        do k = 1, dims_send(pertubation(3)) 
            do j = 1, dims_send(pertubation(2)) 
                do i = 1, dims_send(pertubation(1)) 
                    work_space3D_send(i, j, k) =  grid3D_pointer_send(j, i, k)
                end do
                call MPI_Gather(sendbuf   = work_space3D_send(:,j, k), &
                                sendcount = send_count, &
                                sendtype  = MPI_DOUBLE, &
                                recvbuf   = grid3D_pointer_rcv(:,rcv_j(j), k), &
                                recvcount = send_count, &
                                recvtype  = MPI_DOUBLE, &
                                root      = root(j), &
                                comm      = self%MPI_Comm_Row, &
                                ierror    = ierr0)
            end do
        end do

        ! Cleanup --------------------------------------------------------------
        if (sum(ierr) /= 0) error stop "Grid Row 213 Failed"

        if (allocated(ierr)) deallocate(ierr, stat = ierr0)
        if (ierr0 /= 0) print *, "ierr: Deallocation request denied, grid_row_213"

        if (allocated(root)) deallocate(root, stat = ierr0)
        if (ierr0 /= 0) print *, "ierr: Deallocation request denied, grid_row_213"

        if (allocated(rcv_j)) deallocate(rcv_j, stat = ierr0)
        if (ierr0 /= 0) print *, "ierr: Deallocation request denied, grid_row_213"

    end subroutine rotate_grid_row_213_cpu

    subroutine rotate_grid_col_321_cpu(self, grid_handler_send, grid_handler_rcv, overwrite)
        ! Parameters================================================================
        class(Grid3D_Comm_Handler)         :: self
        class(Grid3D_cpu), intent(inout)   :: grid_handler_send, grid_handler_rcv
        logical:: overwrite
        integer                            :: send_count, &
                                              my_rank, &
                                              comm_dim, &
                                              ierr0, i, j, k, m, n
        real(kind = wp), pointer, &
                         dimension(:,:,:):: work_space3D_send, grid3D_pointer_send, grid3D_pointer_rcv
        integer, dimension(3)            :: dims_send, &
                                            dims_rcv, &
                                            pertubation = [3, 2, 1], &
                                            subgrid_factors_xyz = [1, 1, 1], &
                                            subgrid_dividers_xyz = [1, 1, 1]
        integer, dimension(:), &
                 allocatable             :: ierr, root, rcv_j
        real(kind = wp), pointer, dimension(:):: send_buf_pointer, work_space_send
        TYPE(MPI_Request):: request
        ! Notes=====================================================================
        ! Each process A, B, C has to scatter their data to all other processes in its row:
        ! Look at one (1, 2)-surface:
        !       A     B     C 
        !    |1 1 1|2 2 2|3 3 3|    A |1 1 1 2 2 2 3 3 3|
        !    |     |     |     |      |-----------------|
        !    |1 1 1|2 2 2|3 3 3| -> B |1 1 1 2 2 2 3 3 3|
        !    |     |     |     |      |-----------------|
        !    |1 1 1|2 2 2|3 3 3|    C |1 1 1 2 2 2 3 3 3|
        ! Body======================================================================
        call MPI_Comm_rank(self%MPI_COMM_CART, my_rank)
        call grid_handler_send%get_switch_dims_workspace( &
                dims_send, &
                work_space3D_send, &
                work_space_send, &
                grid3D_pointer_send, &
                pertubation)

        subgrid_factors_xyz(pertubation(1))  = self%column_size
        subgrid_dividers_xyz(pertubation(3)) = self%column_size

        call grid_handler_rcv%perturb_state( &
            state_xyz            = grid_handler_send%state_xyz, &
            grid_xyz_dims        = grid_handler_send%grid_xyz_dims, &
            subgrid_factors_xyz  = subgrid_factors_xyz, &
            subgrid_dividers_xyz = subgrid_dividers_xyz, &
            perturbation_xyz     = pertubation) 

        call grid_handler_rcv%get_pointer_3D(grid3D_pointer_rcv)
        dims_rcv = grid_handler_rcv%get_dims()

        send_count = dims_send(pertubation(1))  ! surface area/self%row_size

        ! Defining root and rcv_j and ierr for the communication later ---------
        allocate(root(dims_send(pertubation(3))), stat = ierr0) 
        if (ierr0 /= 0) print *, "ierr(dim(3)): Allocation request denied"
        allocate(rcv_j(dims_send(pertubation(3))), stat = ierr0)
        if (ierr0 /= 0) print *, "ierr(dim(3)): Allocation request denied"
        allocate(ierr(dims_send(pertubation(3))*self%column_size), stat = ierr0)
        if (ierr0 /= 0) print *, "ierr(dim(3)): Allocation request denied"
        ierr = 0

        k = 0
        j = dims_send(pertubation(3))/self%column_size
        do i = 1, dims_send(pertubation(3)), j
            root(i:i+j-1) = k
            k = k+1
            n = 1
            do m = 1, j
               rcv_j(i+m-1) = n
                n = n+1
            end do
        end do

        ! Communication loop ---------------------------------------------------
        do k = 1, dims_send(1)  
            do j = 1, dims_send(2)       
                do i = 1, dims_send(3)  
                    work_space3D_send(i, j, k) =  grid3D_pointer_send(k, j, i)
                end do

            call MPI_Gather(SENDBUF   = WORK_SPACE3d_SEND(:,j, k), &
                            sendcount = send_count, &
                            sendtype  = MPI_DOUBLE, &
                            recvbuf   = grid3D_pointer_rcv(:, j, rcv_j(k)), &
                            recvcount = send_count, &
                            recvtype  = MPI_DOUBLE, &
                            root      = root(k), &
                            comm      = self%MPI_Comm_Column, &
                            ierror    = ierr0)
            end do
        end do

        ! Cleanup --------------------------------------------------------------
        if (sum(ierr) /= 0) error stop "Grid Row 213 Failed"

        if (allocated(ierr)) deallocate(ierr, stat = ierr0)
        if (ierr0 /= 0) print *, "ierr: Deallocation request denied, grid_row_213"

        if (allocated(root)) deallocate(root, stat = ierr0)
        if (ierr0 /= 0) print *, "ierr: Deallocation request denied, grid_row_213"

        if (allocated(rcv_j)) deallocate(rcv_j, stat = ierr0)
        if (ierr0 /= 0) print *, "ierr: Deallocation request denied, grid_row_213"
    end subroutine rotate_grid_col_321_cpu

end module grid_comm_module
