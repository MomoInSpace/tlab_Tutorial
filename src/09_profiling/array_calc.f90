! In this program we want to explain 
! - how to allocate a matrix
! - pointers
! - allocatable matrices

program matrix_multi_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    ! use MATRIX_OUTPUT
    use MATRIX_MULTIPLY_MOD
    use EXPORT_ARRAYS
    implicit none 

    ! Matrix Definition:
    integer :: dim1, dim2
    integer :: index1, index2, index3

    ! Time measurement:
    integer :: maxsize = 1000
    integer :: step    = 100
    integer, parameter:: timesteps = 1000/100
    integer :: count, rate
    real(wp)    :: timeAtStart, timeAtEnd
    real(wp)    :: time(6,timesteps)


    do index3 = 100,maxsize,step
        dim1 = index3
        dim2 = index3
        time(1,index3/step)= index3

        ! Allocate x,y,z and fill x and y with values
        allocate(x(dim1,dim2))
        allocate(y(dim1,dim2))
        allocate(z(dim1,dim2))
        call fill_xy_values()

        ! Write out x and y:
        ! call write_out_matrixform(x)
        ! call write_out_matrixform(y)

        ! Matmul_cpu_slow--------------------------------------
        ! Clock Start:
        call system_clock(count = count, count_rate = rate)
        timeAtStart = count / real(rate)

        ! Function:
        call matmul_cpu_slow(x,y,z)

        ! Clock Stop:
        call system_clock(count = count, count_rate = rate)
        timeAtEnd = count / real(rate)
        
        ! Save Time:
        time(2,index3/step)=timeAtEnd-timeAtStart
        ! call write_out_matrixform(z)
        !------------------------------------------------------

        ! Matmul_cpu_dotmix--------------------------------------
        ! Clock Start:
        call system_clock(count = count, count_rate = rate)
        timeAtStart = count / real(rate)

        ! Function:
        call matmul_cpu_dotmix(x,y,z)

        ! Clock Stop:
        call system_clock(count = count, count_rate = rate)
        timeAtEnd = count / real(rate)
        
        ! Save Time:
        time(3,index3/step)=timeAtEnd-timeAtStart
        ! call write_out_matrixform(z)
        !------------------------------------------------------

        ! Matmul_cpu_intrinsic--------------------------------------
        ! Clock Start:
        call system_clock(count = count, count_rate = rate)
        timeAtStart = count / real(rate)

        ! Function:
        call matmul_cpu_intrinsic(x,y,z)

        ! Clock Stop:
        call system_clock(count = count, count_rate = rate)
        timeAtEnd = count / real(rate)
        
        ! Save Time:
        time(4,index3/step)=timeAtEnd-timeAtStart
        ! call write_out_matrixform(z)
        !------------------------------------------------------

        ! Matmul_cpu_acc_kernels--------------------------------------
        ! Clock Start:
        call system_clock(count = count, count_rate = rate)
        timeAtStart = count / real(rate)

        ! Function:
        call matmul_cpu_acc_kernels(x,y,z)

        ! Clock Stop:
        call system_clock(count = count, count_rate = rate)
        timeAtEnd = count / real(rate)
        
        ! Save Time:
        time(5,index3/step)=timeAtEnd-timeAtStart
        ! call write_out_matrixform(z)
        !------------------------------------------------------

        ! Matmul_cpu_acc_loop--------------------------------------
        ! Clock Start:
        call system_clock(count = count, count_rate = rate)
        timeAtStart = count / real(rate)

        ! Function:
        call matmul_cpu_acc_loop(x,y,z)

        ! Clock Stop:
        call system_clock(count = count, count_rate = rate)
        timeAtEnd = count / real(rate)
        
        ! Save Time:
        time(6,index3/step)=timeAtEnd-timeAtStart
        ! call write_out_matrixform(z)
        !------------------------------------------------------

        ! Deallocate x,y,z
        deallocate(x)
        deallocate(y)
        deallocate(z)
    end do

    ! Save time:
    call save_matrix(time,"time.csv")

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