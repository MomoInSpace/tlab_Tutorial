module grid_comm_module
    use mpi_f08
    use grid_handler
    implicit none

    private:: get_factors
    private:: get_task_dims

    type:: Grid3D_Comm_Handler
        ! Grid3D_Communicator  handles the communication of the 3D grid.

        TYPE(MPI_Comm):: MPI_COMM_CART, MPI_Comm_Row, MPI_Comm_Column
        integer              :: world_size, row_size, column_size
        integer              :: cart_rank, row_rank, column_rank
        integer, dimension(2):: MPI_Cart_Dims, MPI_Cart_Coords

    contains
        procedure:: init
        ! procedure:: rotate_grid_row_213
        ! procedure:: rotate_grid_col_312

        !Only For Debugging:
        ! procedure:: gather_grid
        ! procedure:: allocate_gather_buffer
        ! procedure:: reorder_gatherv
    end type Grid3D_Comm_Handler


contains

    subroutine init(self, world_size, vertical_dimension)
        class(Grid3D_Comm_Handler):: self
        integer              :: world_size, rank, vertical_dimension
        integer, dimension(2):: task_dims  
        logical, dimension(2):: periods = [.false., .false.]
        ! Error integer:
        integer, dimension(100):: ierr  = 0

        call get_task_dims(world_size, vertical_dimension, self%MPI_Cart_Dims)

        call MPI_CART_CREATE(MPI_COMM_WORLD, 2, self%MPI_Cart_Dims, periods, .true., self%MPI_COMM_CART, ierr(1)) 

        call MPI_Comm_rank(self%MPI_COMM_CART, self%cart_rank)
        call MPI_CART_COORDS(self%MPI_COMM_CART, self%cart_rank, 2, self%MPI_Cart_Coords, ierr(2))

        ! Create Row communicator
        call MPI_Comm_split(self%MPI_COMM_CART, self%MPI_Cart_Coords(2), self%MPI_Cart_Coords(1), self%MPI_Comm_Row, ierr(3))

        ! Create Column communicator
        call MPI_Comm_split(self%MPI_COMM_CART, self%MPI_Cart_Coords(1), self%MPI_Cart_Coords(2), self%MPI_Comm_Column, ierr(4))
        
    end subroutine init

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
