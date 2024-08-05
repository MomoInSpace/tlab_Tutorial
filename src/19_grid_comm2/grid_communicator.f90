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
        integer, dimension(2):: MPI_Cart_Dims, MPI_Cart_Coords

    contains
        procedure:: init
        procedure:: rotate_grid_row_213
        procedure:: rotate_grid_col_312

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

        call MPI_Comm_rank(MPI_COMM_CART, rank)
        call MPI_CART_COORDS(MPI_COMM_CART, rank, 2, MPI_Cart_Coords, ierr(2))

        ! Create Row communicator
        call MPI_Comm_split(MPI_COMM_CART, MPI_Cart_Coords(2), MPI_Cart_Coords(1), self%MPI_Comm_Row, ierr(3))

        ! Create Column communicator
        call MPI_Comm_split(MPI_COMM_CART, MPI_Cart_Coords(1), MPI_Cart_Coords(2), self%MPI_Comm_Column, ierr(4))
        
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

    function prod(arr)
        integer, dimension(:), intent(in):: arr
        integer:: prod
        integer:: i

        prod = 1  ! Initialize product

        do i = 1, size(arr)
            prod = prod*arr(i)
        end do
    end function prod

    ! subroutine gather_grid(self, subgrid, complete_grid_array, buffer_array)
    !     class(Grid3D_Comm_Handler):: self
    !     class(Grid3D) :: subgrid
    !     real(kind = wp), asynchronous, &
    !                      dimension(:), &
    !                      allocatable, target   :: complete_grid_array, buffer_array
    !     integer :: rank
    !     integer, dimension(2):: task_state
    !     integer, dimension(3):: state_xyz, grid_xyz_dims
    !     integer, dimension(100):: ierr  = 0

    !     call MPI_Comm_rank(self%MPI_COMM_CART, rank)

    !     if(rank == 0) then
    !         call allocate_complete_grid(subgrid, complete_grid_array, buffer_array)
    !     end if


    !     send_num = prod(grid_xyz_dims)
    !     call MPI_Gather(sendbuf    = subgrid%grid_space, &
    !                     sendcount  = send_num, &
    !                     sendtype   = MPI_DOUBLE, &
    !                     recvbuf    = buffer_array, &
    !                     recvcount  = send_num, &
    !                     recvtype   = MPI_DOUBLE, &
    !                     root       = 0, &
    !                     comm       = MPI_COMM_CART, &
    !                     ierror     = ierr(3))

    ! ! Write For Testing
    !  if (my_rank == 0) then 
    !     call testgrid_handler%reorder_gatherv()!, dims_tasks_2d, subgrid_xyz_dims)
    !  end if

    ! if (sum(ierr) /= 0) print *, "Something went terribly wrong in gather_grid"


    ! end subroutine gather_grid

    ! subroutine allocate_complete_grid(subgrid, complete_grid_array, buffer_array)
    !     class(Grid3D) :: subgrid
    !     real(kind = wp), asynchronous, &
    !                      dimension(:), &
    !                      allocatable, target   :: complete_grid_array, buffer_array
    !     integer :: rank
    !     integer, dimension(2):: task_state
    !     integer, dimension(3):: state_xyz, grid_xyz_dims, dims, dims_complete
    !     integer, dimension(2):: ierr  = 0
    !     integer:: n_max, m_max
    !         task_state = [2, 1] ! If you use MPI_COMM_CART, use [2, 1]
    !         ! Initiate Test Grid
    !         ! First we initialize the total grid as subgrid
    !         grid_xyz_dims = subgrid%grid_xyz_dims
    !         state_xyz = subgrid%state_xyz
    !         dims = get_dims()
    !         MPI_Cart_Dims = self%MPI_Cart_Dims


    !         dims_complete(1) = grid_xyz_dims(state_xyz(1))
    !         dims_complete(2) = grid_xyz_dims(state_xyz(2))
    !         dims_complete(3) = grid_xyz_dims(state_xyz(3))
    !                         
    !         ! Now we multiply the last two dimensions of the grid with 
    !         ! the dimensions of the tasks in said direction
    !         n_max = MPI_Cart_Dims(task_state(1))
    !         m_max = MPI_Cart_Dims(task_state(2))
    !         grid_xyz_dims(state_xyz(2)) = grid_xyz_dims(state_xyz(2))*n_max
    !         grid_xyz_dims(state_xyz(3)) = grid_xyz_dims(state_xyz(3))*m_max


    !     ! Allocate the grid_array with length prod(grid_xyz_dims)
    !     allocate( &
    !              grid_array(prod(grid_xyz_dims)), stat = ierr(1) &
    !     )
    !     allocate( &
    !              buffer_array(prod(grid_xyz_dims)), stat = ierr(2) &
    !     )

    !     if (sum(ierr)/= 0) error stop "subgrid grid_array: Allocation request denied"

    !     call reorder_gatherv(dims, n_max, m_max, dims_complete )
    ! end subroutine allocate_complete_grid

    ! subroutine reorder_gatherv(dims, n_max, m_max, dims_complete)!, dims_tasks_2d, grid_xyz_dims)!, state, grid_array, buffer_array)
    !     real(kind = wp), pointer, &
    !                      dimension(:,:,:):: buffer_pointer_3d
    !     real(kind = wp), pointer, &
    !                      dimension(:):: buffer_pointer_1d
    !     ! class(Complete_Grid), intent(in)          :: self
    !     integer, dimension(3)           :: dims, dims_complete
    !     integer:: i, j, k, m, n, p, n_max, m_max
    !     ! integer, dimension(2):: dims_tasks_2d
    !     ! integer, dimension(3):: grid_xyz_dims
    !     integer, dimension(2)               :: ierr
    !     character(len = 100):: fmt


    !     
    !     p = 1
    !     call self%get_sub_dims(dims)

    !     do m = 1, m_max  
    !         do n = 1, n_max  
    !             do i = 1, dims(3)  
    !                 do j = 1, dims(2)  
    !                     self%grid_pointer_3d(:, j+dims(2)*(n-1), i+dims(3)*(m-1)) = &
    !                     self%buffer_pointer_1d(p:p+dims(1))
    !                     p = p+dims(1)  
    !                 end do
    !             end do
    !         end do
    !     end do


    !     ! write(*,*) "topmost xz-surface of total grid, with shape:"
    !     write(*,*) "State:   ", self%state_xyz
    !     call self%get_sub_dims(dims)
    !     write(*,*) "Subgrid: ", dims
    !     call self%get_dims(dims)
    !     write(*,*) "Dims:    ", dims
    !     ! write(*,*) "Dims2:"

    !     write(*,*) "Dims (1, :,:)"
    !     write(fmt, '(A, I0, A)') '(', dims(2), 'F4.0)'
    !     write(*,fmt) self%grid_pointer_3d(1, :,:)

    !     write(*,*) "Dims (:,1, :)"
    !     write(fmt, '(A, I0, A)') '(', dims(1), 'F4.0)'
    !     write(*,fmt) self%grid_pointer_3d(:, 1, :)

    !     write(*,*) "Dims (:,:,1)"
    !     write(fmt, '(A, I0, A)') '(', dims(1), 'F4.0)'
    !     write(*,fmt) self%grid_pointer_3d(:, :, 1)
    !     ! write(*,*) "Dims3:"
    !     ! write(fmt, '(A, I0, A)') '(', dims(3), 'F4.0)'
    !     ! write(*,fmt) self%grid_pointer_3d(1, :,:)
    !     ! write(*,*) self%grid_pointer_3d(1, :,:)
    !     ! write(*,*) shape(self%grid_pointer_3d(1, :,:))

    ! end subroutine reorder_gatherv

    ! procedure:: gather_grid
    ! procedure:: allocate_gather_buffer
    ! procedure:: reorder_gatherv


end module grid_comm_module
