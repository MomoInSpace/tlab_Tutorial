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
    integer, parameter :: maxsize = 1500
    integer, parameter :: step    = 100
    integer, parameter :: timesteps = maxsize/step 
    integer :: count, rate
    real(wp)    :: timeAtStart, timeAtEnd
    real(wp)    :: time(7,timesteps)


    do index3 = step,maxsize,step
        dim1 = index3
        dim2 = index3
        time(1,index3/step)= index3
        print *, "Loop ", index3/step, " of ", maxsize/step

        ! Allocate x,y,z and fill x and y with values
        allocate(x(dim1,dim2))
        allocate(y(dim1,dim2))
        allocate(z(dim1,dim2))
        call fill_xy_values()

        ! Write out x and y:
        ! call write_out_matrixform(x)
        ! call write_out_matrixform(y)

        ! Matmul_cpu_slow--------------------------------------
        call time_matmul_calc(2,time(2,index3/step))

        ! Matmul_cpu_dotmix--------------------------------------
        call time_matmul_calc(3,time(3,index3/step))

        ! Matmul_cpu_intrinsic--------------------------------------
        call time_matmul_calc(4,time(4,index3/step))

        ! Matmul_gpu_acc_kernels--------------------------------------
        call time_matmul_calc(5,time(5,index3/step))

        ! Matmul_gpu_acc_loop--------------------------------------
        call time_matmul_calc(6,time(6,index3/step))

        !gpu dotmix-------------------------------------------------
        call time_matmul_calc(7,time(7,index3/step))

        !gpu intrinsic ---------------------------------------------
        ! call time_matmul_calc(8,time(8,index3/step))

        ! Deallocate x,y,z
        deallocate(x)
        deallocate(y)
        deallocate(z)
    end do

    ! Save time:
    call write_out_matrixform(time)
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

    subroutine time_matmul_calc(type, calc_time)
        integer, intent(in) :: type
        real(wp), intent(out) :: calc_time
    
        ! Clock Start:
        call system_clock(count = count, count_rate = rate)
        timeAtStart = count / real(rate)

        ! Function selection:
        select case (type)
        case default
            print *, "No Case Selected! ERROR!"
        case (2) !matmul slow
            call matmul_cpu_slow(x,y,z)
        case (3) !cpu dotmix
            call matmul_cpu_dotmix(x,y,z)
        case (4) !cpu intrinsic
            call matmul_cpu_intrinsic(x,y,z)
        case (5) !gpu acc kernels
            call matmul_cpu_acc_kernels(x,y,z)
        case (6) !gpu acc loop
            call matmul_cpu_acc_loop(x,y,z)
        case (7) !gpu dotmix
            call matmul_gpu_dotmix(x,y,z)
        ! case (8) !gpu intrinsic
        !     call matmul_gpu_intrinsic(x,y,z)
        end select

        ! Clock Stop:
        call system_clock(count = count, count_rate = rate)
        timeAtEnd = count / real(rate)
        
        ! Calculate Time:
        calc_time = timeAtEnd-timeAtStart
        
    end subroutine time_matmul_calc

end program matrix_multi_test
