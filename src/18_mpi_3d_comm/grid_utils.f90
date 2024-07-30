module grid_utils
    use mpi_f08
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

    subroutine get_task_dims(world_size, y_s, task_dims)
    ! Minimizes max(dims_task_2d), 
    !   while keeping in mind, that dims_task_2d(1) <= y_s
    !   and sum(task_dims) = world_size.
    !   For flat grids, this leads to task_dims(1) = y_s

    implicit none
    ! Parameters================================================================
    integer, intent(in):: world_size, y_s
    ! task_dims are the dimensions of the cartesian tasks 
    integer, dimension(2), intent(out):: task_dims  
    integer:: i, j, fac, m_num, n_num, factor_diff, my_rank
    integer, allocatable:: m_arr(:), n_arr(:)
    integer, allocatable:: factors_ys(:), factors_wsize(:)

    ! Body======================================================================
    ! Compute factors of y_s

    factors_ys = get_factors(y_s)
    factors_wsize = get_factors(world_size)

    n_num = world_size
    m_num = 1
    do i = 1, size(factors_ys)
        fac = factors_ys(i)
        if (any(factors_wsize == fac) .and. (fac+world_size/fac <= n_num+m_num) )then
            n_num = fac
            m_num = world_size/n_num
            end if
    end do

    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    if (my_rank == 0) then
        if (n_num /= y_s) print *, "Note: n_num not divisible by y_s. For flat grids this reduces communication delay! "
        if (n_num /= y_s) print *, "n_num: ", n_num, ", y_s: ", y_s
    end if
    if (n_num == 1 .or. m_num == 1) error stop "Fatal: world_size ist not divisible by facors of y_s, except 1, or calc error. Check code"
    if (n_num == 0) error stop "Fatal: Something went terribly wrong. n_num in get_task_dims shouldn't be zero!!"

    task_dims = [m_num, n_num]

    end subroutine get_task_dims

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
                write(*,'(I4)' , advance='no') cube(i, j, 1)
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

subroutine gather_and_print_characters(my_chars, MPI_print_comm)
    use mpi_f08
    implicit none

    ! Arguments
    character(len=*), intent(in):: my_chars   ! Each process's character array

    ! Locals
    integer:: ierr, my_rank, num_procs, root, i
    character(len = 3000):: gathered_chars   ! Character array to hold gathered results
    TYPE(MPI_Comm):: MPI_print_comm

    call MPI_COMM_RANK(MPI_print_comm, my_rank, ierr)
    call MPI_COMM_SIZE(MPI_print_comm, num_procs, ierr)

    root = 0

    ! Allocate space to receive all character arrays at the root process
    ! if (my_rank == root) then
    !     allocate(gathered_chars(num_procs*size(my_chars)))
    ! end if

    ! Gather all character arrays at the root
    call MPI_GATHER(my_chars, len(my_chars), MPI_CHARACTER, &
                    gathered_chars, len(my_chars), MPI_CHARACTER, root, MPI_print_comm, ierr)

    ! Root process prints the gathered characters in order
    if (my_rank == root) then
        print*, "Gathered characters in order:"
        do i = 1, num_procs
            ! write(*,*) "noice"
            write(*,*,advance = 'no') gathered_chars((i-1)*len(my_chars)+1:i*len(my_chars))
        end do
        ! deallocate(gathered_chars)  ! Deallocate dynamically allocated array
    end if

end subroutine gather_and_print_characters

end module grid_utils

! TEST:
! program task_dims
!     use grid_utils
!     implicit none

!     integer, dimension(2):: task_dimss  
!     integer:: t_nn, y_s, t_node, n_fin, m_fin

!     ! Example values
!     t_nn = 33
!     y_s = 6
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
