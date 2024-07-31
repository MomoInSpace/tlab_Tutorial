module compgrid_handler 
    use TLAB_CONSTANTS, only: wp
    use grid_utils
    use mpi_f08
    use grid_handler
    implicit none

    type, extends(Grid):: Complete_Grid 
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
    end type Complete_Grid

contains

    subroutine init_complete(self, state_xyz, grid_xyz_dims, subgrid_xyz_dims, task_dims, task_state)
        class(Complete_Grid)                      :: self
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
        class(Complete_Grid)                      :: self
        integer, dimension(3), intent(inout)           :: dims

        dims(1) = self%subgrid_xyz_dims(self%state_xyz(1))
        dims(2) = self%subgrid_xyz_dims(self%state_xyz(2))
        dims(3) = self%subgrid_xyz_dims(self%state_xyz(3))

    end subroutine get_sub_dims

    subroutine allocate_arrays_wbuffer(self, grid_array, buffer_array)
        class(Complete_Grid), intent(in)          :: self
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
        class(Complete_Grid), intent(in)          :: self
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
        ! write(*,*) "Dims3:"
        ! write(fmt, '(A, I0, A)') '(', dims(3), 'F4.0)'
        ! write(*,fmt) self%grid_pointer_3d(1, :,:)
        ! write(*,*) self%grid_pointer_3d(1, :,:)
        ! write(*,*) shape(self%grid_pointer_3d(1, :,:))

    end subroutine reorder_gatherv
  
end module compgrid_handler 
