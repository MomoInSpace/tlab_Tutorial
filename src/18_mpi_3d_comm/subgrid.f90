module subgrid_handler 
    use TLAB_CONSTANTS, only: wp
    use grid_utils
    use mpi_f08
    implicit none

  ! Define the Point type
    type:: Subgrid
        integer                      :: state
        integer, dimension(3)        :: subgrid_xyz_dims
        real(kind = wp), pointer, &
                         dimension(:,:,:):: grid_pointer, buffer_pointer

    contains
        procedure:: init
        procedure:: get_state_string
        procedure:: set_pointer
        procedure:: allocate_arrays
    end type Subgrid 

contains

    subroutine init(self, state, subgrid_xyz_dims)
        class(Subgrid)                      :: self
        integer, intent(in)                 :: state 
        integer, intent(in), &
                 dimension(3)               :: subgrid_xyz_dims
        ! real(kind = wp), intent(out),  &
        !                  asynchronous, &
        !                  dimension(:), &
        !                  allocatable        :: grid_array, buffer
        integer, dimension(2):: ierr 

        self%state = state
        self%subgrid_xyz_dims = subgrid_xyz_dims


    end subroutine init

    subroutine allocate_arrays(self, state, grid_array, buffer_array)
        class(Subgrid), intent(in)          :: self
        integer, intent(in)                 :: state
        real(kind = wp), intent(inout),   &
                         asynchronous, &
                         dimension(:), &
                         allocatable, target:: grid_array, buffer_array
        integer, dimension(2)               :: ierr

        ! Allocate the grid_array with length prod(subgrid_xyz_dims)
        allocate( &
                 grid_array(prod(self%subgrid_xyz_dims)), stat = ierr(1) &
        )
        allocate( &
                 buffer_array(prod(self%subgrid_xyz_dims)), stat = ierr(2) &
    )

    if (sum(ierr)/= 0) error stop "subgrid grid_array: Allocation request denied"

    call self%set_pointer(state, grid_array, buffer_array)

    end subroutine allocate_arrays

    subroutine set_pointer(self, state, grid_array, buffer_array)
        class(Subgrid)                      :: self
        integer, intent(in)                 :: state
        integer                             :: dim1, dim2, dim3
        real(kind = wp), intent(in),   &
                         asynchronous, &
                         dimension(:), &
                         allocatable, target        :: grid_array, buffer_array

        select case(self%state)
            case(1)
                dim1 = self%subgrid_xyz_dims(1)
                dim2 = self%subgrid_xyz_dims(2)
                dim3 = self%subgrid_xyz_dims(3)
            case(2)
                dim1 = self%subgrid_xyz_dims(2)
                dim2 = self%subgrid_xyz_dims(3)
                dim3 = self%subgrid_xyz_dims(1)
            case(3)
                dim1 = self%subgrid_xyz_dims(3)
                dim2 = self%subgrid_xyz_dims(1)
                dim3 = self%subgrid_xyz_dims(2)
        end select

        self%grid_pointer(1:dim1, &
                          1:dim2, &
                          1:dim3) => grid_array

        self%buffer_pointer(1:dim1, &
                            1:dim2, &
                            1:dim3) => buffer_array

    end subroutine set_pointer

    subroutine get_state_string(self, print_bool, state_string)
        class(Subgrid), intent(in)   :: self
        character(3), intent(out), &
                      optional       :: state_string
        logical, intent(in), &
                 optional            :: print_bool

        select case(self%state)
            case(1)
                state_string = "xyz"
            case(2)
                state_string = "yzx"
            case(3)
                state_string = "zxy"
        end select

        if (present(print_bool)) then
            if(print_bool) write(*,* , advance='no') state_string
        end if

    end subroutine get_state_string
  
end module subgrid_handler 
