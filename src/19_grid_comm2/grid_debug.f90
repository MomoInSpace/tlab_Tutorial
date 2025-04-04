module grid_debug 
    use TLAB_CONSTANTS, only: wp
    use mpi_f08

    use grid_handler
    use grid_comm_module
    implicit none

    ! private:: prod

  ! Define the Point type
    type:: Grid_debugger
        integer, dimension(3)        :: state_xyz
        integer, dimension(3)        :: grid_xyz_dims
        integer                      :: overhead_factor, free_space
        real(kind = wp), pointer, &
                         dimension(:,:,:):: grid_pointer_3d
        real(kind = wp), pointer, &
                         dimension(:):: grid_pointer_1d, overhead_pointer_1d, complete_pointer_1d

    contains
        procedure:: init
        procedure:: get_state_string
        procedure:: set_pointer
        procedure:: allocate_arrays
        procedure:: get_dims
        procedure:: switch_dims_12
        procedure:: reorder_gatherv_sub
    end type Grid_debugger

    type, extends(Grid_debugger):: Complete_Grid_debugger
        real(kind = wp), pointer, &
                         dimension(:,:,:):: buffer_pointer_3d
        real(kind = wp), pointer, &
                         dimension(:):: buffer_pointer_1d
        integer, dimension(3)        :: subgrid_xyz_dims
        integer, dimension(2)        :: task_dims, task_state

        contains
            procedure:: init_complete
            procedure:: get_sub_dims
            procedure:: allocate_arrays_wbuffer
            procedure:: reorder_gatherv
            procedure:: gather_compgrid
    end type Complete_Grid_debugger

contains

    subroutine init(self, state_xyz, grid_xyz_dims)
        class(Grid_debugger)                      :: self
        integer, intent(in), &
                 dimension(3)               :: state_xyz
        integer, intent(in), &
                 dimension(3)               :: grid_xyz_dims
        integer, dimension(2):: ierr 

        self%overhead_factor = 2
        self%state_xyz = state_xyz
        self%grid_xyz_dims = grid_xyz_dims

    end subroutine init

    subroutine allocate_arrays(self, grid_array)
        class(Grid_debugger), intent(inout)          :: self
        ! integer, intent(in), &
        !          dimension(3)               :: state_xyz
        real(kind = wp), intent(inout),   &
                         asynchronous, &
                         dimension(:), &
                         allocatable, target:: grid_array
        integer                             :: ierr
        integer                             :: max_area, free_space, total_space


        max_area = max(prod(self%grid_xyz_dims(1:2)), &
                       prod(self%grid_xyz_dims(2:3)), &
                       prod(self%grid_xyz_dims(1:3:2)))
        free_space = max_area*self%overhead_factor
        total_space = free_space+prod(self%grid_xyz_dims)

        self%free_space = free_space


        ! Allocate the grid_array with length prod(grid_xyz_dims)
        allocate(grid_array(total_space), stat = ierr)

    if (ierr /= 0) error stop "subgrid grid_array: Allocation request denied"

    ! self%overhead_pointer_1d => grid_array(:free_space)
    self%complete_pointer_1d => grid_array
    call self%set_pointer(self%grid_pointer_1d, self%grid_pointer_3d, &
                          grid_array(free_space:))

    end subroutine allocate_arrays

    subroutine get_dims(self, dims)
        class(Grid_debugger)                      :: self
        integer, dimension(3), intent(inout)           :: dims

        dims(1) = self%grid_xyz_dims(self%state_xyz(1))
        dims(2) = self%grid_xyz_dims(self%state_xyz(2))
        dims(3) = self%grid_xyz_dims(self%state_xyz(3))

    end subroutine get_dims

    subroutine set_pointer(self, grid_pointer_1d, grid_pointer_3d, grid_array)
        class(Grid_debugger)                      :: self
        integer, dimension(3)            :: dims
        real(kind = wp), pointer, &
                         dimension(:)    :: grid_pointer_1d
        real(kind = wp), pointer, &
                         dimension(:,:,:):: grid_pointer_3d 
        real(kind = wp), intent(in),   &
                         asynchronous, &
                         dimension(:), &
                         target        :: grid_array 
                         ! allocatable, target        :: grid_array 

        ! dims = self%state_xyz
        call self%get_dims(dims)

        grid_pointer_1d => grid_array
        grid_pointer_3d(1:dims(1), &
                             1:dims(2), &
                             1:dims(3)) => grid_array

    end subroutine set_pointer

    subroutine switch_dims_12(self)
        class(Grid_debugger), intent(inout):: self
        integer, dimension(3)            :: dims
        real(kind = wp), pointer, &
                         dimension(:,:):: surface 
        integer:: i
        real(kind = wp):: placeholder


        call self%get_dims(dims)
        
        do i = 1, dims(3), 1
            ! surface => reshape(self%complete_pointer_1d(dim(1)*dim(2)*(i-1)+1:dim(1)*dim(2)*i), [dim(2), dim(1)])
             call dns_transpose(self%grid_pointer_1d(dims(1)*dims(2)*(i-1):dims(1)*dims(2)*i), & 
                                dims(1), dims(2), dims(2), &
                                self%complete_pointer_1d(dims(1)*dims(2)*(i-1):dims(1)*dims(2)*i), & 
                                dims(2))
        end do

        self%state_xyz = [dims(2), dims(1), dims(3)]
        call self%set_pointer(self%grid_pointer_1d, self%grid_pointer_3d, &
                              self%complete_pointer_1d(:self%free_space))
        
    end subroutine switch_dims_12

    subroutine get_state_string(self, print_bool, state_string)
        class(Grid_debugger), intent(in)   :: self
        character(len = 3), &
                      optional       :: state_string
        logical, intent(in), &
                 optional            :: print_bool
        integer, dimension(3)               :: dims

        ! dims = self%state_xyz
        call self%get_dims(dims)
        state_string(dims(1):dims(1)) = "x"
        state_string(dims(2):dims(2)) = "y"
        state_string(dims(3):dims(3)) = "z"

        ! select case(self%state)
        !     case(1)
        !         state_string = "xyz"
        !     case(2)
        !         state_string = "yzx"
        !     case(3)
        !         state_string = "zxy"
        ! end select

        if (present(print_bool)) then
            if(print_bool) write(*,* , advance='no') state_string
        end if

    end subroutine get_state_string

    subroutine reorder_gatherv_sub(self, len_comm, grid_pointer_3d)!, dims_tasks_2d, grid_xyz_dims)!, state, grid_array, buffer_array)
        class(Grid_debugger), intent(in)                     :: self
        real(kind = wp), intent(out), &
                         pointer, dimension(:,:,:)  :: grid_pointer_3d 
        integer, dimension(3)                       :: dims
        integer:: i, j, k, m, n, p, n_max, m_max, len_comm
        ! integer, dimension(2):: dims_tasks_2d
        ! integer, dimension(3):: grid_xyz_dims
        integer, dimension(2)               :: ierr
        character(len = 100):: fmt
        
        p = 1
        n_max = 1  ! self%task_dims(self%task_state(1))
        m_max = len_comm  ! self%task_dims(self%task_state(2))
        call self%get_dims(dims)

        do m = 1, m_max  
            do n = 1, n_max  
                do i = 1, dims(3)  
                    do j = 1, dims(2)  
                        grid_pointer_3d(:, j+dims(2)*(n-1), i+dims(3)*(m-1)) = &
                        self%grid_pointer_1d(p:p+dims(1))
                        p = p+dims(1)  
                    end do
                end do
            end do
        end do


        ! write(*,*) "topmost xz-surface of total grid, with shape:"
        ! write(*,*) "State:   ", self%state_xyz
        ! call self%get_dims(dims)
        ! write(*,*) "Subgrid: ", dims
        ! call self%get_dims(dims)
        ! write(*,*) "Dims:    ", dims
        ! ! write(*,*) "Dims2:"

        ! write(*,*) "Dims (1, :,:)"
        ! write(fmt, '(A, I0, A)') '(', dims(2), 'F4.0)'
        ! write(*,fmt) self%grid_pointer_3d(1, :,:)

        ! write(*,*) "Dims (:,1, :)"
        ! write(fmt, '(A, I0, A)') '(', dims(1), 'F4.0)'
        ! write(*,fmt) self%grid_pointer_3d(:, 1, :)

        ! write(*,*) "Dims (:,:,1)"
        ! write(fmt, '(A, I0, A)') '(', dims(1), 'F4.0)'
        ! write(*,fmt) self%grid_pointer_3d(:, :, 1)
        ! write(*,*) "Dims3:"
        ! write(fmt, '(A, I0, A)') '(', dims(3), 'F4.0)'
        ! write(*,fmt) self%grid_pointer_3d(1, :,:)
        ! write(*,*) self%grid_pointer_3d(1, :,:)
        ! write(*,*) shape(self%grid_pointer_3d(1, :,:))

    end subroutine reorder_gatherv_sub

    subroutine dns_transpose(a, nra, nca, ma, b, mb)
        use TLAB_CONSTANTS
        implicit none

        integer(wi), intent(in):: nra      ! Number of rows in a
        integer(wi), intent(in):: nca      ! Number of columns in b
        integer(wi), intent(in):: ma       ! Leading dimension on the input matrix a
        integer(wi), intent(in):: mb       ! Leading dimension on the output matrix b
        real(wp), intent(in)    :: a(ma, *)  ! Input array
        real(wp), intent(out)   :: b(mb, *)  ! Transposed array

    ! -------------------------------------------------------------------
        integer(wi) jb, kb

        integer(wi):: srt, end, siz

        integer(wi) k, j, jj, kk
        integer(wi) last_k, last_j

        parameter(jb = 64, kb = 64)
    ! -------------------------------------------------------------------

        ! call DNS_OMP_PARTITION(nca, srt, end, siz)
          ! omp_siz = len 
          ! omp_srt = 1 
          ! omp_end = len
        ! siz = nca
        srt = 1
        end = nca

        kk = 1; jj = 1

        do k = srt, end-kb+1, kb; 
            do j = 1, nra-jb+1, jb; 
                do jj = j, j+jb-1
                    do kk = k, k+kb-1
                        b(kk, jj) = a(jj, kk)
                    end do
                end do
            end do
        end do

        last_k = kk
        last_j = jj

        do k = last_k, end
            do j = 1, nra
                b(k, j) = a(j, k)
            end do
        end do

        do k = srt, end
            do j = last_j, nra
                b(k, j) = a(j, k)
            end do
        end do



        return
    end subroutine dns_transpose

    subroutine init_complete(self, state_xyz, grid_xyz_dims, subgrid_xyz_dims, task_dims, task_state)
        class(Complete_Grid_debugger)                      :: self
        integer, intent(in), &
                 dimension(3)               :: state_xyz
        integer, intent(in), &
                 dimension(3)               :: grid_xyz_dims, subgrid_xyz_dims
        integer, dimension(2)               :: ierr 
        integer, intent(in), &
                 dimension(2)               :: task_dims, task_state

        self%state_xyz = state_xyz
        self%grid_xyz_dims = grid_xyz_dims
        self%subgrid_xyz_dims = subgrid_xyz_dims
        self%task_dims = task_dims
        self%task_state = task_state

    end subroutine init_complete

    subroutine get_sub_dims(self, dims)
        class(Complete_Grid_debugger)                      :: self
        integer, dimension(3), intent(inout)           :: dims

        dims(1) = self%subgrid_xyz_dims(self%state_xyz(1))
        dims(2) = self%subgrid_xyz_dims(self%state_xyz(2))
        dims(3) = self%subgrid_xyz_dims(self%state_xyz(3))

    end subroutine get_sub_dims

    subroutine allocate_arrays_wbuffer(self, grid_array, buffer_array)
        class(Complete_Grid_debugger), intent(in)          :: self
        ! integer, intent(in), &
        !          dimension(3)               :: state_xyz
        real(kind = wp), intent(inout),   &
                         asynchronous, &
                         dimension(:), &
                         allocatable, target:: grid_array, buffer_array
        integer, dimension(2)               :: ierr

        ! Allocate the grid_array with length prod(grid_xyz_dims)
        allocate( &
                 grid_array(prod(self%grid_xyz_dims)), stat = ierr(1) &
        )
        allocate( &
                 buffer_array(prod(self%grid_xyz_dims)), stat = ierr(2) &
    )

    if (sum(ierr)/= 0) error stop "subgrid grid_array: Allocation request denied"

    call self%set_pointer(self%grid_pointer_1d, self%grid_pointer_3d, grid_array)
    call self%set_pointer(self%buffer_pointer_1d, self%buffer_pointer_3d, buffer_array)

    end subroutine allocate_arrays_wbuffer

    subroutine reorder_gatherv(self)!, dims_tasks_2d, grid_xyz_dims)!, state, grid_array, buffer_array)
        class(Complete_Grid_debugger), intent(in)          :: self
        integer, dimension(3)           :: dims
        integer:: i, j, k, m, n, p, n_max, m_max
        ! integer, dimension(2):: dims_tasks_2d
        ! integer, dimension(3):: grid_xyz_dims
        integer, dimension(2)               :: ierr
        character(len = 100):: fmt
        
        p = 1
        n_max = self%task_dims(self%task_state(1))
        m_max = self%task_dims(self%task_state(2))
        call self%get_sub_dims(dims)

        do m = 1, m_max  
            do n = 1, n_max  
                do i = 1, dims(3)  
                    do j = 1, dims(2)  
                        self%grid_pointer_3d(:, j+dims(2)*(n-1), i+dims(3)*(m-1)) = &
                        self%buffer_pointer_1d(p:p+dims(1))
                        p = p+dims(1)  
                    end do
                end do
            end do
        end do


        ! write(*,*) "topmost xz-surface of total grid, with shape:"
        write(*,*) "=========================================================="
        write(*,*) "State:   ", self%state_xyz
        call self%get_sub_dims(dims)
        write(*,*) "Subgrid: ", dims
        call self%get_dims(dims)
        write(*,*) "Dims:    ", dims
        ! write(*,*) "Dims2:"

        write(*,*) "Dims (1, :,:)"
        write(fmt, '(A, I0, A)') '(', dims(2), 'F4.0)'
        write(*,fmt) self%grid_pointer_3d(1, :,:)

        write(*,*) "Dims (:,1, :)"
        write(fmt, '(A, I0, A)') '(', dims(1), 'F4.0)'
        write(*,fmt) self%grid_pointer_3d(:, 1, :)

        write(*,*) "Dims (:,:,1)"
        write(fmt, '(A, I0, A)') '(', dims(1), 'F4.0)'
        write(*,fmt) self%grid_pointer_3d(:, :, 1)
        ! write(*,*) ".........................................................."
        ! write(*,*) "Dims3:"
        ! write(fmt, '(A, I0, A)') '(', dims(3), 'F4.0)'
        ! write(*,fmt) self%grid_pointer_3d(1, :,:)
        ! write(*,*) self%grid_pointer_3d(1, :,:)
        ! write(*,*) shape(self%grid_pointer_3d(1, :,:))

    end subroutine reorder_gatherv

    ! function prod(arr)
    !     integer, dimension(:), intent(in):: arr
    !     integer:: prod
    !     integer:: i

    !     prod = 1  ! Initialize product

    !     do i = 1, size(arr)
    !         prod = prod*arr(i)
    !     end do
    ! end function prod

    subroutine gather_compgrid(testgrid_handler, grid_handler, grid_comm_handler, &
                               subgrid_xyz_dims, my_rank)
                               ! testgrid_array  , testbuffer_array, &

        type(Grid3D_cpu)                        :: grid_handler
        type(Grid3D_Comm_Handler)               :: grid_comm_handler
        Class(Complete_grid_debugger)            :: testgrid_handler
        integer, dimension(3)                   :: state_xyz
        integer, dimension(3)                   :: subgrid_xyz_dims, grid_xyz_dims 
        integer                                 ::  my_rank

        ! Debug arrays
        real(kind = wp), &
                         asynchronous, &
                         dimension(:), &
                         allocatable, target   :: testgrid_array, testbuffer_array
        INTEGER, DIMENSION(2):: dims_tasks_2d
        integer                                :: send_num, err
        real(kind = wp), pointer, &
                         dimension(:):: grid_pointer_1d

        state_xyz  = grid_handler%state_xyz
        dims_tasks_2d = grid_comm_handler%MPI_CART_DIMS

        ! Calculate grid_xyz_dims--------------------------------------------------
        subgrid_xyz_dims = grid_handler%grid_xyz_dims
        grid_xyz_dims = [subgrid_xyz_dims(1), &                                    
                         subgrid_xyz_dims(2), &
                         subgrid_xyz_dims(3)]

        ! Now we multiply the last two dimensions of the grid with 
        ! the dimensions of the tasks in said direction
        grid_xyz_dims(state_xyz(2)) = grid_xyz_dims(state_xyz(2))*grid_comm_handler%row_size  
        grid_xyz_dims(state_xyz(3)) = grid_xyz_dims(state_xyz(3))*grid_comm_handler%column_size  

        ! Initialization of testgrid_handler---------------------------------------
        call testgrid_handler%init_complete(state_xyz, grid_xyz_dims, &
                                            subgrid_xyz_dims, &
                                            dims_tasks_2d, &
                                            [2, 1])  ! The [2, 1] value is always the same.
                                                     ! this should be removed.

        call testgrid_handler%allocate_arrays_wbuffer(testgrid_array, testbuffer_array)

        ! Gather complete Grid-----------------------------------------------------
        send_num = prod(grid_handler%grid_xyz_dims)
        call MPI_Gather(sendbuf    = grid_handler%grid_space, &
                        sendcount  = send_num, &
                        sendtype   = MPI_DOUBLE, &
                        recvbuf    = testgrid_handler%buffer_pointer_1d, &
                        recvcount  = send_num, &
                        recvtype   = MPI_DOUBLE, &
                        root       = 0, &
                        comm       = grid_comm_handler%MPI_COMM_CART, &
                        ierror     = err)

        ! Reorder The buffer so that it has the correct form------------------------
         if (my_rank == 0) then 
            call testgrid_handler%reorder_gatherv()
         end if

        ! Allocate testgrid and testbuffer . . . . . . . . . . . . . . . . . . . . 
        if (allocated(testgrid_array)) deallocate(testgrid_array, stat = err)
        if (err /= 0) print *, "array: Deallocation request denied"

        if (allocated(testbuffer_array)) deallocate(testbuffer_array, stat = err)
        if (err /= 0) print *, "array: Deallocation request denied"

    end subroutine gather_compgrid

end module grid_debug 

