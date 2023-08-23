module parameters
    use kind_parameter
    implicit none

    ! Integer loop param
    integer(i8):: n_value
    integer(i8):: k_value

    ! Grid params:
    integer(i8),parameter:: steps_x = 1000
    integer(i8),parameter:: steps_z = 1000

    real(dp), allocatable :: x(:)
    real(dp), allocatable :: z(:)

    target :: x,z

    real(dp), parameter :: x_min =  0., &
                           x_max =  1000
    real(dp), parameter :: z_min = -10000, &
                           z_max =  10000

    
contains

    subroutine init_grid()
        allocate(x(steps_x))
        allocate(z(steps_z))

        x = (/(x_min + (x_max-x_min)*real(n_value)/real(steps_x), &
            n_value=1, &
            steps_x)/)

        z = (/(z_min + (z_max-z_min)*real(n_value)/real(steps_z), &
            n_value=1, &
            steps_z)/) 
    end subroutine init_grid
    
    
end module parameters