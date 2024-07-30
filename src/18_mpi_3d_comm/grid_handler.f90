module grid_handler 
    use TLAB_CONSTANTS, only: wp
    use grid_utils
    use mpi_f08
    implicit none

  ! Define the Point type
    type:: Grid
        integer, dimension(3)        :: state_xyz
        integer, dimension(3)        :: grid_xyz_dims
        real(kind = wp), pointer, &
                         dimension(:,:,:):: grid_pointer_3d 
        real(kind = wp), pointer, &
                         dimension(:):: grid_pointer_1d

    contains
        procedure:: init
        procedure:: get_state_string
        procedure:: set_pointer
        procedure:: allocate_arrays
        procedure:: get_dims
    end type Grid 

contains

    subroutine init(self, state_xyz, grid_xyz_dims)
        class(Grid)                      :: self
        integer, intent(in), &
                 dimension(3)               :: state_xyz
        integer, intent(in), &
                 dimension(3)               :: grid_xyz_dims
        integer, dimension(2):: ierr 

        self%state_xyz = state_xyz
        self%grid_xyz_dims = grid_xyz_dims

    end subroutine init

    subroutine allocate_arrays(self, grid_array)
        class(Grid), intent(in)          :: self
        ! integer, intent(in), &
        !          dimension(3)               :: state_xyz
        real(kind = wp), intent(inout),   &
                         asynchronous, &
                         dimension(:), &
                         allocatable, target:: grid_array
        integer, dimension(2)               :: ierr

        ! Allocate the grid_array with length prod(grid_xyz_dims)
        allocate( &
                 grid_array(prod(self%grid_xyz_dims)), stat = ierr(1) &
        )

    if (sum(ierr)/= 0) error stop "subgrid grid_array: Allocation request denied"

    call self%set_pointer(self%grid_pointer_1d, self%grid_pointer_3d, grid_array)

    end subroutine allocate_arrays

    subroutine get_dims(self, dims)
        class(Grid)                      :: self
        integer, dimension(3), intent(inout)           :: dims

        dims(1) = self%grid_xyz_dims(self%state_xyz(1))
        dims(2) = self%grid_xyz_dims(self%state_xyz(2))
        dims(3) = self%grid_xyz_dims(self%state_xyz(3))

    end subroutine get_dims

    subroutine set_pointer(self, grid_pointer_1d, grid_pointer_3d, grid_array)
        class(Grid)                      :: self
        integer, dimension(3)            :: dims
        real(kind = wp), pointer, &
                         dimension(:)    :: grid_pointer_1d
        real(kind = wp), pointer, &
                         dimension(:,:,:):: grid_pointer_3d 
        real(kind = wp), intent(in),   &
                         asynchronous, &
                         dimension(:), &
                         allocatable, target        :: grid_array 

        ! dims = self%state_xyz
        call self%get_dims(dims)

        grid_pointer_1d => grid_array
        grid_pointer_3d(1:dims(1), &
                             1:dims(2), &
                             1:dims(3)) => grid_array

    end subroutine set_pointer

    subroutine get_state_string(self, print_bool, state_string)
        class(Grid), intent(in)   :: self
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

end module grid_handler 
