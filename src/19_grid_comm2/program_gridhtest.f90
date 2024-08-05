
program comm_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use mpi_f08
    ! use grid_utils
    use grid_handler
    ! use compgrid_handler
    ! use MATRIX_OUTPUT
    implicit none 

    ! Parameters================================================================
    ! Grid definition, sub_grid_y = [x_cy*, y_s, z_cy*]
    ! INTEGER, DIMENSION(3), target:: sub_grid_y

    integer                                :: send_num
    integer, dimension(3)                  :: state_xyz
    type(Grid)                             :: subgrid_handler 
    type(Complete_Grid)                    :: testgrid_handler
    integer, dimension(3)                  :: subgrid_xyz_dims, grid_xyz_dims 
    integer, dimension(2)                  :: task_state
    real(kind = wp), asynchronous, &
                     dimension(:), &
                     allocatable, target   :: subgrid_array!, subbuffer_array
    real(kind = wp), asynchronous, &
                     dimension(:), &
                     allocatable, target   :: testgrid_array, testbuffer_array
        real(kind = wp), pointer, &
                         dimension(:,:,:)  :: subgrid_pointer => null(),   &
                                             ! subbuffer_pointer => null(), &
                                             testgrid_pointer => null(),  &
                                             testbuffer_pointer => null(), &
                                             temp_pointer => null()

end program comm_test
