module grid_handler 
    use TLAB_CONSTANTS, only: wp
    implicit none

    ! private:: prod

  ! Define the Point type
    type, abstract:: Grid3D
        ! Grid3D handles the structure of the 3D grid.

        integer, dimension(3):: state_xyz
        ! state_xyz describes the orientation of the grid. state_xyz(1) = 2
        !  means, that the second dimension in the structured grid can be
        !  understood as the 'x' coordinate.

        integer, dimension(3):: grid_xyz_dims
        ! grid_xyz_dims saves the length of each dimension x, y and z

        integer:: overhead_factor, free_space, total_space
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
        procedure:: perturb_state
        procedure:: set_pointer_1D
        procedure:: get_pointer_3D
        procedure:: allocate_array
        procedure:: get_switch_dims_213_workspace
        ! procedure:: switch_dims_132_step
        procedure:: print_state
    end type Grid3D

  ! Define the Point type
    type, extends(Grid3D):: Grid3D_cpu 
    ! contains
        ! procedure:: set_pointer_1D => set_pointer_1D_cpu
        ! procedure:: get_pointer_3D => get_pointer_3D_cpu
        ! procedure:: allocate_array => allocate_array_cpu
    end type Grid3D_cpu

contains

    subroutine init(self, state_xyz, grid_xyz_dims, overhead_factor, subgrid_factors)
        class(Grid3D)           :: self
        integer, intent(in), &
                 dimension(3)   :: state_xyz
        integer, intent(in), &
                 dimension(3)   :: grid_xyz_dims, subgrid_factors
        integer                 :: overhead_factor, max_area

        ! Initialisation of Grid3D.=============================================

        self%state_xyz = state_xyz
        self%grid_xyz_dims = grid_xyz_dims*subgrid_factors
        self%overhead_factor = overhead_factor

        max_area = max(prod(self%grid_xyz_dims(1:2)), &
                       prod(self%grid_xyz_dims(2:3)), &
                       prod(self%grid_xyz_dims(1:3:2)))
        self%free_space = max_area*self%overhead_factor
        self%total_space = self%free_space+prod(self%grid_xyz_dims)

    end subroutine init

    function get_dims(self) result(dims)
        class(Grid3D)        :: self
        integer, dimension(3):: dims

        dims(1) = self%grid_xyz_dims(self%state_xyz(1))
        dims(2) = self%grid_xyz_dims(self%state_xyz(2))
        dims(3) = self%grid_xyz_dims(self%state_xyz(3))

    end function get_dims

    subroutine perturb_state(self, state_xyz, grid_xyz_dims, subgrid_factors_123, subgrid_dividers_123, perturbation_123) 
        class(Grid3D)          :: self
        integer, intent(in), &
                 dimension(3)  :: perturbation_123, subgrid_factors_123, subgrid_dividers_123
        integer, dimension(3)  :: state_xyz, grid_xyz_dims
        integer                :: i
        ! As the dimensions should always change with the same factors, the amount of free space stays the same.
        !   This is why whe only have to change the state_xyz and the grid_xyz_dims and not the 1D_pointers.
        ! It is assumed that the Grid3D is already initialized before this function is called!
        ! The Factors are applied first, then the state is perturbed!

        self%grid_xyz_dims(state_xyz(1)) = grid_xyz_dims(state_xyz(1))*subgrid_factors_123(1)/subgrid_dividers_123(1)
        self%grid_xyz_dims(state_xyz(2)) = grid_xyz_dims(state_xyz(2))*subgrid_factors_123(2)/subgrid_dividers_123(2)
        self%grid_xyz_dims(state_xyz(3)) = grid_xyz_dims(state_xyz(3))*subgrid_factors_123(3)/subgrid_dividers_123(3)

        do i = 1, 3
            self%state_xyz(i) = state_xyz(perturbation_123(i))
        end do

    end subroutine perturb_state

    subroutine allocate_array(self, grid_array)
            class(Grid3D), intent(inout)          :: self
            real(kind = wp), intent(inout),   &
                             asynchronous, &
                             dimension(:), &
                             allocatable, target:: grid_array
            integer                             :: ierr
            integer                             :: total_space


            ! Allocate the grid_array with length prod(grid_xyz_dims)
            allocate(grid_array(self%total_space), stat = ierr)
            if (ierr /= 0) error stop "subgrid grid_array: Allocation request denied"

            call self%set_pointer_1D(grid_array)

    end subroutine allocate_array

    subroutine set_pointer_1D(self, grid_array)
        class(Grid3D), intent(inout)          :: self
        real(kind = wp), pointer, &
                         dimension(:):: grid_space, allocated_space
        real(kind = wp), intent(in),   &
                         asynchronous, &
                         dimension(:), &
                         target        :: grid_array 
        integer                        :: total_space

        total_space = self%free_space+prod(self%grid_xyz_dims)
        if (size(grid_array) < total_space) then 
            write(*,*) "Array is too small for grid", size(grid_array), "< ",total_space
            error stop 
        end if

        ! write(*,*) self%free_space, size(grid_array), self%total_space

        self%grid_space => grid_array(self%free_space+1:)  ! INDEXING MIGHT BE WRONG
        self%allocated_space => grid_array

    end subroutine set_pointer_1D

    subroutine get_pointer_3D(self, grid3D_pointer)  ! result(grid3D_pointer)
        class(Grid3D), intent(in)          :: self
        real(kind = wp), intent(inout), &
                         pointer, &
                         dimension(:,:,:):: grid3D_pointer
        integer, dimension(3)            :: dims

        dims = self%get_dims()

        ! write(*,*) dims(1)*dims(2)*dims(3), dims(1), dims(2), dims(3), size(self%grid_space)

        grid3D_pointer(1:dims(1), &
                       1:dims(2), &
                       1:dims(3)) => self%grid_space
    end subroutine get_pointer_3D

    subroutine get_switch_dims_213_workspace(self, dims, work_space, grid_3D_pointer)
        class(Grid3D), intent(in):: self
        real(kind = wp), intent(out), &
                         pointer, &
                         dimension(:,:,:):: work_space, grid_3D_pointer
        integer, intent(out), &
                 dimension(3)            :: dims

        dims = self%get_dims()
        call self%get_pointer_3D(grid_3D_pointer)

        work_space(1:dims(2), &
                   1:dims(1), &
                   1:dims(3)) => &
            self%allocated_space(1+self%free_space-dims(1)*dims(2): &
                                 1+self%free_space+dims(1)*dims(2)*(dims(3)-1))
        ! write(*,*) size(work_space), size(self%grid_space)
 
    end subroutine get_switch_dims_213_workspace

    subroutine print_state(self, state_string)
        class(Grid3D), intent(in)   :: self
        character(len = 3), optional:: state_string
        integer, dimension(3)       :: dims
        real(kind = wp), pointer, &
                         dimension(:,:,:):: grid3D_pointer 
        ! Formating:
        character(len = 100):: fmt

        dims = self%get_dims()
        call self%get_pointer_3D(grid3D_pointer )

        state_string(dims(1):dims(1)) = "x"
        state_string(dims(2):dims(2)) = "y"
        state_string(dims(3):dims(3)) = "z"

        write(*,*) "State: ", state_string

        write(*,*) "Dims (1, :,:)"
        write(fmt, '(A, I0, A)') '(', dims(2), 'F4.0)'
        write(*,fmt) grid3D_pointer(1, :,:)

        write(*,*) "Dims (:,1, :)"
        write(fmt, '(A, I0, A)') '(', dims(1), 'F4.0)'
        write(*,fmt) grid3D_pointer(:, 1, :)

        write(*,*) "Dims (:,:,1)"
        write(fmt, '(A, I0, A)') '(', dims(1), 'F4.0)'
        write(*,fmt) grid3D_pointer(:, :, 1)


        ! write(*,*) grid3D_pointer

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
