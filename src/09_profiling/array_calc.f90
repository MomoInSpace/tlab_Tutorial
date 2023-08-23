! In this program we want to explain 
! - how to allocate a matrix
! - pointers
! - allocatable matrices

program matrix_multi_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use MATRIX_OUTPUT
    use MATRIX_MULTIPLY_MOD, only: matmul_cpu_slow,matmul_cpu_dotmix,matmul_cpu_intrinsic
    implicit none 
    integer :: dim1, dim2
    integer :: index1, index2, index3


    do index3 = 100,1000,100
        dim1 = index3
        dim2 = index3

        ! Allocate x,y,z and fill x and y with values
        allocate(x(dim1,dim2))
        allocate(y(dim1,dim2))
        allocate(z(dim1,dim2))
        call fill_xy_values()

        ! Write out x and y:
        ! call write_out_matrixform(x)
        ! call write_out_matrixform(y)

        call matmul_cpu_slow(x,y,z)
        ! call write_out_matrixform(z)

        call matmul_cpu_dotmix(x,y,z)
        ! call write_out_matrixform(z)

        call matmul_cpu_intrinsic(x,y,z)
        ! call write_out_matrixform(z)

        ! Deallocate x,y,z
        deallocate(x)
        deallocate(y)
        deallocate(z)
    end do

    contains

    subroutine fill_xy_values()
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    implicit none
    ! Fill x and y with values
    do index1 = 1, dim1
        do index2 = 1, dim2
            x(index1,index2) = 1
            if ( index1==index2 ) then
                y(index1,index2) = index1
            end if
        end do
    end do
    end subroutine fill_xy_values

end program matrix_multi_test




! program main
! use MatrixMultiplyModule
! implicit none

! real,allocatable        ::  a(:,:),b(:,:)
! real,allocatable    ::  c1(:,:),c2(:,:),c3(:,:)
! integer ::  n
! integer ::  count, rate
! real    ::  timeAtStart, timeAtEnd
! real    ::  time(3,10)
! do n=100,1000,100
!     allocate(a(n,n),b(n,n))

!     call random_number(a)
!     call random_number(b)

!     call system_clock(count = count, count_rate = rate)
!     timeAtStart = count / real(rate)
!     call LoopMatrixMultiply(a,b,c1)
!     call system_clock(count = count, count_rate = rate)
!     timeAtEnd = count / real(rate)
!     time(1,n/100)=timeAtEnd-timeAtStart

!     call system_clock(count = count, count_rate = rate)
!     timeAtStart = count / real(rate)
!     call IntrinsicMatrixMultiply(a,b,c2)
!     call system_clock(count = count, count_rate = rate)
!     timeAtEnd = count / real(rate)
!     time(2,n/100)=timeAtEnd-timeAtStart

!     call system_clock(count = count, count_rate = rate)
!     timeAtStart = count / real(rate)
!     call MixMatrixMultiply(a,b,c3)
!     call system_clock(count = count, count_rate = rate)
!     timeAtEnd = count / real(rate)
!     time(3,n/100)=timeAtEnd-timeAtStart


!     deallocate(a,b)

! end do

! open(1,file="time.txt")
! do n=1,10
!     write(1,*) time(:,n)
! end do
! close(1)
! deallocate(c1,c2,c3)
! end program