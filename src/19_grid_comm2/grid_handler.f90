module grid_handler 
    use TLAB_CONSTANTS, only: wp
    implicit none

    private:: prod

  ! Define the Point type
    type, abstract:: Grid3D
        ! Grid3D handles the structure of the 3D grid.

        integer, dimension(3):: state_xyz
        ! state_xyz describes the orientation of the grid. state_xyz(1) = 2
        !  means, that the second dimension in the structured grid can be
        !  understood as the 'x' coordinate.

        integer, dimension(3):: grid_xyz_dims
        ! grid_xyz_dims saves the length of each dimension x, y and z

        integer:: overhead_factor, free_space
        ! overhead gives you the factor of the overhead that needs to be
        !  there for a fast communication. A factor of 2 means that we need
        !  2 surfaces space, i.e. structured_grid(:,:,:+2)
        ! free_space is the differene of indices between 
        !  grid_space and allocated_space

        real(kind = wp), pointer, &
                         dimension(:):: grid_space, allocated_space
        ! grid_space points to the space of the grid.
        ! allocated_space points to the grid AND the overhead.

    contains
        procedure:: init
        procedure:: get_dims
        procedure:: set_pointer_1D
        procedure:: get_pointer_3D
        procedure:: allocate_array
        ! procedure:: switch_dims_213_step
        ! procedure:: switch_dims_132_step
        procedure:: print_state
    end type Grid3D

  ! Define the Point type
    type, extends(Grid3D):: Grid3D_cpu 
    contains
        procedure:: set_pointer_1D => set_pointer_1D_cpu
        procedure:: get_pointer_3D => get_pointer_3D_cpu
        procedure:: allocate_array => allocate_array_cpu
    end type Grid3D_cpu

contains

    subroutine init(self, state_xyz, grid_xyz_dims, overhead_factor)
        class(Grid3D)           :: self
        integer, intent(in), &
                 dimension(3)   :: state_xyz
        integer, intent(in), &
                 dimension(3)   :: grid_xyz_dims
        integer                 :: overhead_factor, max_area

        ! Initialisation of Grid3D.=============================================

        self%state_xyz = state_xyz
        self%grid_xyz_dims = grid_xyz_dim
        self%overhead_factor = overhead_factor

        max_area = max(prod(self%grid_xyz_dims(1:2)), &
                       prod(self%grid_xyz_dims(2:3)), &
                       prod(self%grid_xyz_dims(1:3:2)))
        self%free_space = max_area*self%overhead_factor

    end subroutine init

    function get_dims(self) result(dims)
        class(Grid)                      :: self
        integer, dimension(3), intent(out)           :: dims

        dims(1) = self%grid_xyz_dims(self%state_xyz(1))
        dims(2) = self%grid_xyz_dims(self%state_xyz(2))
        dims(3) = self%grid_xyz_dims(self%state_xyz(3))

    end function get_dims

    subroutine allocate_array_cpu(self, grid_array)
            class(Grid3D_cpu), intent(inout)          :: self
            real(kind = wp), intent(inout),   &
                             asynchronous, &
                             dimension(:), &
                             allocatable, target:: grid_array
            integer                             :: ierr
            integer                             :: total_space


            total_space = self%free_space+prod(self%grid_xyz_dims)

            ! Allocate the grid_array with length prod(grid_xyz_dims)
            allocate(grid_array(total_space), stat = ierr)
            if (ierr /= 0) error stop "subgrid grid_array: Allocation request denied"

            call self%set_pointer_1D(grid_array)

    end subroutine allocate_array_cpu

    subroutine set_pointer_1D_cpu(self, grid_array)
        class(Grid3D_cpu), intent(inout)          :: self
        real(kind = wp), pointer, &
                         dimension(:):: grid_space, allocated_space
        real(kind = wp), intent(in),   &
                         asynchronous, &
                         dimension(:), &
                         target        :: grid_array 
        integer                        :: total_space

        total_space = self%free_space+prod(self%grid_xyz_dims)
        if (len(grid_array) < total_space) error stop "Array is too small for grid"
        self%grid_space => grid_array(self%free_space:)
        self%allocated_space => grid_array

    end subroutine set_pointer_1D_cpu

    subroutine get_pointer_3D_cpu(self, grid3D_pointer)
        class(Grid3D_cpu), intent(inout)          :: self
        real(kind = wp), pointer, &
                         dimension(:,:,:):: grid3D_pointer 
        integer, dimension(3)            :: dims

        dims = self%get_dims()

        grid3D_pointer(1:dims(1), &
                       1:dims(2), &
                       1:dims(3)) => grid_array
    end subroutine get_pointer_3D_cpu

    subroutine print_state(self, state_string)
        class(Grid3D), intent(in)   :: self
        character(len = 3), optional:: state_string
        integer, dimension(3)       :: dims

        dims = self%get_dims()

        state_string(dims(1):dims(1)) = "x"
        state_string(dims(2):dims(2)) = "y"
        state_string(dims(3):dims(3)) = "z"

        write(*,*) "State: " state_string

    end subroutine print_state 

    ! Private Subourtines and Functions ========================================
    ! ==========================================================================

    function prod(arr)
        integer, dimension(:), intent(in):: arr
        integer:: prod
        integer:: i

        prod = 1  ! Initialize product

        do i = 1, size(arr)
            prod = prod*arr(i)
        end do
    end function prod

end module grid_handler 
