module grid_utils
  implicit none

contains

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

    subroutine get_nm_from_ys(world_size, y_s, task_dims)
    ! Minimizes max(dims_task_2d), 
    !   while keeping in mind, that dims_task_2d(1) <= y_s
    !   and sum(task_dims) = world_size.
    !   For flat grids, this leads to task_dims(1) = y_s

    implicit none
    ! Parameters================================================================
    integer, intent(in):: world_size, y_s
    ! task_dims are the dimensions of the cartesian tasks 
    integer, dimension(2), intent(out):: task_dims  
    integer:: i, fac, m_num, my_factor
    integer, allocatable:: m_arr(:), n_arr(:)
    integer, allocatable:: factors(:)
    integer:: n_fact

    ! Body======================================================================
    ! Compute factors of y_s
    factors = get_factors(y_s)

    ! Arrays to store permissible m and n values
    allocate(m_arr(0))
    allocate(n_arr(0))

    ! Iterate over each factor
    do i = 1, size(factors)
        fac = factors(i)
        m_num = world_size-fac
        if (m_num > 0) then
            n_arr = [n_arr, fac]
            m_arr = [m_arr, m_num]
        endif
    enddo

    ! Initialize my_factor and result variables
    task_dims(1) = 0
    task_dims(2) = 0
    my_factor = abs(n_arr(1) - m_arr(1))

    ! Find the closest pair of m, n
    do i = 1, size(n_arr)
        n_fact = n_arr(i)
        m_num = m_arr(i)
        if (abs(m_num-n_fact) <= my_factor) then
            my_factor = abs(m_num-n_fact)
            task_dims(1) = n_fact
            task_dims(2) = m_num
        endif
    enddo

    end subroutine get_nm_from_ys

    subroutine print_cube_views(cube, n, m, p)
        implicit none
        integer, intent(in):: n, m, p
        integer, intent(in):: cube(n, m, p)
        integer:: i, j, k

        ! Print the top view
        print *, "Top View:"
        do j = 1, m
            ! Print top view slice
            do i = 1, n
                write(*, '(I4)', advance='no') cube(i, j, 1)
            end do
            write(*, *)  ! Newline after each row
        end do
        write(*,*)

        write(*,*) "Front View: "
        do k = 1, p
            ! Print front view slice
            do i = 1, n
                write(*, '(I4)', advance='no') cube(i, m, k)
            end do
            write(*, *)  ! Newline after each row
        end do
        write(*,*)

        write(*,*) "Left View: "
        do k = 1, p
            ! Print left view slice
            do j = 1, m
                write(*, '(I4)', advance='no') cube(1, j, k)
            end do
            write(*, *)  ! Newline after each row
        end do
        write(*,*)
        
        ! Print the bottom delimiter
        call print_bottom_delimiter(n)

    end subroutine print_cube_views

    ! Subroutine to print the bottom delimiter
    subroutine print_bottom_delimiter(columns)
        integer, intent(in):: columns
        integer:: k

        do k = 1, columns
            write(*, '(A)', advance='no') '------'
        end do
        write(*, *)  ! Newline
    end subroutine print_bottom_delimiter

    function prod(arr)
        integer, dimension(:), intent(in):: arr
        integer:: prod
        integer:: i

        prod = 1  ! Initialize product

        do i = 1, size(arr)
            prod = prod*arr(i)
        end do
    end function prod

end module grid_utils

! TEST:
! program task_dims
!     use grid_utils
!     implicit none

!     integer, dimension(2):: task_dimss  
!     integer:: t_nn, y_s, t_node, n_fin, m_fin

!     ! Example values
!     t_nn = 2
!     y_s = 2
!     t_node = 1

!     call get_nm_from_ys(t_nn*t_node, y_s, task_dimss)
!     n_fin = task_dimss(1)
!     m_fin = task_dimss(2)

!     print *, "n_fin =", n_fin
!     print *, "m_fin =", m_fin

! end program task_dims

! Test Cubes:
! program test_cube_sides
!     use grid_utils
!     implicit none

!     integer, parameter:: n = 4, m = 4, p = 4
!     integer:: cube(n, m, p)
!     integer:: i, j, k

!     ! Initialize the 3D array with some values
!     do k = 1, p
!         do j = 1, m
!             do i = 1, n
!                 !cube(i, j, k) = (k-1) * n*n + (j-1) * n+i
!                 cube(i, j, k) = i+ (j-1)*n  ! i+j-1!(j-1) * n+i
!             end do
!         end do
!     end do

!     ! Call the subroutine to print the sides
!     call print_cube_views(cube, n, m, p)

! end program test_cube_sides
