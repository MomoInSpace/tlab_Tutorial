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

    subroutine init(self, state_xyz, grid_xyz_dims, &
                          overhead_factor, subgrid_factors)
        ! Parameters================================================================
        class(Grid3D)           :: self
        integer, intent(in), &
                 dimension(3)   :: state_xyz, grid_xyz_dims, subgrid_factors
        integer                 :: overhead_factor, max_area
        ! Notes=====================================================================
        ! Initialization of Grid3D. Setting the base values. 
        ! ALWAYS has to be called first before using Grid3D.
        ! Body======================================================================

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

    subroutine perturb_state(self, state_xyz, grid_xyz_dims, &
                             subgrid_factors_123, subgrid_dividers_123, &
                             perturbation_123) 
        ! Parameters================================================================
        class(Grid3D), intent(inout):: self
        integer, intent(in), &
             dimension(3)           :: state_xyz, grid_xyz_dims, &
                                       perturbation_123, subgrid_factors_123, &
                                       subgrid_dividers_123
        ! Loop variables
        integer                     :: i
        ! Notes=====================================================================
        ! As the dimensions should always change with the same factors, the amount 
        !   of free space stays the same. This is why whe only have to change the 
        !   state_xyz and the grid_xyz_dims and not the 1D_pointers.
        ! It is assumed that the Grid3D is already initialized, 
        !   before this function is called!
        ! The Factors are applied first, then the state is perturbed!
        ! When calling this subroutine it is assumed that the data in 
        !   self%allocated_space can be disregarded and overwritten.
        ! Body======================================================================

        ! Resizing of the xyz-axes:
        self%grid_xyz_dims(state_xyz(1)) = &
            grid_xyz_dims(state_xyz(1))*subgrid_factors_123(1)/subgrid_dividers_123(1)

        self%grid_xyz_dims(state_xyz(2)) = &
            grid_xyz_dims(state_xyz(2))*subgrid_factors_123(2)/subgrid_dividers_123(2)

        self%grid_xyz_dims(state_xyz(3)) = &
            grid_xyz_dims(state_xyz(3))*subgrid_factors_123(3)/subgrid_dividers_123(3)

        ! Perturbation of xyz-axes:
        do i = 1, 3
            self%state_xyz(i) = state_xyz(perturbation_123(i))
        end do

    end subroutine perturb_state

    subroutine allocate_array(self, grid_array)
        ! Parameters================================================================
            class(Grid3D),   intent(inout)  :: self
            integer                         :: total_space
            real(kind = wp), intent(inout), &
                             allocatable,   &
                             asynchronous,  &
                             dimension(:),  &
                             target         :: grid_array
            ! Error handling
            integer                         :: ierr
        ! Notes=====================================================================
        ! The array can be allocated outside this subroutine. The pointer can be
        !   set with an existing array, provided it has the right size.
        ! Body======================================================================


        ! Allocate the grid_array with length prod(grid_xyz_dims)
        allocate(grid_array(self%total_space), stat = ierr)
        if (ierr /= 0) error stop "subgrid grid_array: Allocation request denied"

        call self%set_pointer_1D(grid_array)

    end subroutine allocate_array

    subroutine set_pointer_1D(self, grid_array)
        ! Parameters================================================================
        class(Grid3D), intent(inout)          :: self
        real(kind = wp), pointer, &
                         dimension(:):: grid_space, allocated_space
        real(kind = wp), intent(in),   &
                         asynchronous, &
                         dimension(:), &
                         target        :: grid_array 
        integer                        :: total_space
        ! Notes=====================================================================
        ! Body======================================================================

        total_space = self%free_space+prod(self%grid_xyz_dims)
        if (size(grid_array) < total_space) then 
            write(*,*) "Array is too small for grid", size(grid_array), "< ",total_space
            error stop 
        end if

        self%grid_space => grid_array(self%free_space+1:)  ! INDEXING MIGHT BE WRONG
        self%allocated_space => grid_array

    end subroutine set_pointer_1D

    subroutine get_pointer_3D(self, grid3D_pointer)  
        ! Parameters================================================================
        class(Grid3D), intent(in)          :: self
        real(kind = wp), intent(inout), &
                         pointer, &
                         dimension(:,:,:):: grid3D_pointer
        integer, dimension(3)            :: dims
        ! Notes=====================================================================
        ! In each state, the cuboids have the same volume. This is why we can
        !   retrieve the 3D pointer by just using get_dims(), whithout needing
        !   to adjust self%grid_space
        ! Body======================================================================

        dims = self%get_dims()

        grid3D_pointer(1:dims(1), &
                       1:dims(2), &
                       1:dims(3)) => self%grid_space
    end subroutine get_pointer_3D

    subroutine get_switch_dims_213_workspace(self, dims, work_space, grid_3D_pointer)
        ! Parameters================================================================
        class(Grid3D),   intent(in)   :: self
        integer,         intent(out), &
           dimension(3)               :: dims
        real(kind = wp), intent(out), &
             pointer, &
             dimension(:,:,:)         :: work_space, grid_3D_pointer
        ! Notes=====================================================================
        ! The base assumption when initializing our subgrids with the grid_handler
        !   is, that we have 'overhead_factor's of free layers of (:,:,1) with no 
        !   data and then grid-data below it. The workspace is then one layer 
        !   higher, i.e:
        !   Grid data: grid_array(:,:,overhead_factor:)
        !   Grid data: grid_array(:,:,overhead_factor-1:)
        ! 
        !   grid_3D_pointer       work_space:
        !   0 0 0                 0 0 0    The 0 marks unused allocated space, the 
        !   0 0 0                 x x x        x marks used space.
        !   x x x    ------>      x x x 
        !   x x x                 0 0 0 
        !
        ! We also switch the dimensions 1 and 2.
        ! Body======================================================================

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
        ! Parameters================================================================
        class(Grid3D), intent(in)   :: self
        character(len = 3), optional:: state_string
        integer, dimension(3)       :: dims
        real(kind = wp), pointer, &
                         dimension(:,:,:):: grid3D_pointer 
        ! Formating:
        character(len = 100):: fmt
        ! Notes=====================================================================
        ! Subroutine to print the sides of the subgrid cube.
        ! Body======================================================================

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

    end subroutine print_state 

    ! Private Subourtines and Functions ========================================
    ! ==========================================================================

    function prod(arr)
        ! Parameters================================================================
        integer, dimension(:), intent(in):: arr
        integer:: prod
        integer:: i
        ! Notes=====================================================================
        ! Body======================================================================

        prod = 1  ! Initialize product

        do i = 1, size(arr)
            prod = prod*arr(i)
        end do
    end function prod

end module grid_handler 
