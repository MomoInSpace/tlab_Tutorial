! In this program we want to explain 
! - how to allocate a matrix
! - pointers
! - allocatable matrices

program matrix_multi_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use RING_TIMER_MODULE
    use EXPORT_ARRAYS
    !use cudafor
    implicit none 
    ! Parameters======================================== ====================
    CHARACTER(len=32) :: arg
    INTEGER :: arg_num
    CHARACTER(len=10000) :: filename

    ! Matrix Definition:
    integer :: dim1, dim2
    integer :: i, index1, index2, index3
    integer :: maxsize
    integer :: step

    ! Time measurement:
    integer :: count, rate
    real(wp)    :: timeAtStart, timeAtEnd
    integer     :: timesteps! = maxsize/step 
    real(wp), allocatable :: time(:,:)!(3,timesteps)

    ! MPI Parameters
    integer :: my_rank, size
    integer :: maxstep_ring 

    ! Body==================================================================
    ! Initialisation--------------------------------------------------------

    ! MPI Initialisation
    CALL MPI_Init()
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)

    filename = "run_times"
    maxsize = 1000
    step = 100
    maxstep_ring = size

    DO i = 1, COMMAND_ARGUMENT_COUNT()
      CALL getarg(i, arg)
        select case (i)
            case (1) 
                READ(arg, '(I10)') maxsize 
            case (2)
                READ(arg, '(I10)') step
            case (3)
                READ(arg, '(I10)') maxstep_ring
        end select
        WRITE(filename, '(A,A,A)') trim(filename), "_", arg
    END DO
    WRITE(filename, '(A,A)') trim(filename), ".csv"
    timesteps = maxsize/step 
    allocate(time(3,timesteps))

    !WRITE(*,*) trim(filename)

    !call get_command_argument(1, args(1)); maxsize      = ichar(args(1))
    !call get_command_argument(1, args(2)); step         = ichar(args(2))
    !call get_command_argument(1, args(3)); maxstep_ring = ichar(args(3))
    



    do index3 = step,maxsize,step
        dim1 = index3
        dim2 = index3
        time(1,index3/step)= index3

        if (my_rank == 0) then
            print *, "Loop ", index3/step, " of ", maxsize/step
        end if

        ! Allocate x, y, x_dev, y_dev and define as Id-Matrices 
        allocate(x(dim1,dim2))
        allocate(y(dim1,dim2))
        allocate(z(dim1,dim2))

        ! Write out x and y:
        ! call write_out_matrixform(x)
        ! call write_out_matrixform(y)

        ! ring_cpu_slow
        !call fill_xyz_values()
        !call time_ring(2,time(2,index3/step), dim1, dim2)

        ! ring_gpu_device
        call fill_xyz_values()
        call MPI_BARRIER(MPI_COMM_WORLD)
        call time_ring(3,time(2,index3/step), dim1, dim2)
        call check_trace(x)

        ! ring_gpu_acc
        !call fill_xyz_values()
        !call time_ring(4,time(2,index3/step), dim1, dim2)

        ! ring_cpu_fast
        call fill_xyz_values()
        call MPI_BARRIER(MPI_COMM_WORLD)
        call time_ring(5,time(3,index3/step), dim1, dim2)
        call check_trace(x)

        if (my_rank == 0) then
            print *, ""
        end if


        ! Deallocate x,y,z
        deallocate(x)
        deallocate(y)
        deallocate(z)


        ! Save time:
        !call write_out_matrixform(time)
        IF (my_rank == 0) THEN
            call save_array(time(1:3,index3/step),filename)
        END IF

    end do

    deallocate(time)


    CALL MPI_Finalize()

    contains

    subroutine fill_xyz_values()
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    implicit none
    ! Fill x and y with values
    do index2 = 1, dim1
        do index1 = 1, dim2
            call RANDOM_NUMBER(x(index1,index2))
            call RANDOM_NUMBER(y(index1,index2))
            call RANDOM_NUMBER(z(index1,index2))
            if ( index1==index2 ) then
                x(index1,index2) = 0
                y(index1,index2) = 1
                z(index1,index2) = 1
            end if
        end do
    end do
    end subroutine fill_xyz_values

    subroutine check_trace(mat)
        use TLAB_CONSTANTS, only: wp
        use TLAB_ARRAYS 
        implicit none
        REAL(wp), INTENT(IN), DIMENSION(dim1,dim2) :: mat
        REAL(wp) :: mat_val_real
        INTEGER :: mat_val, true_val 

        mat_val = 0
        mat_val_real = 0
        true_val = 0

        ! Works only if size > maxstep_ring > 0
        do index1 = size-1, size - maxstep_ring, -1 
            true_val = true_val + index1 
        end do 
            
        ! Fill x and y with values
        do index1 = 1, dim1
           mat_val_real = mat_val_real + mat(index1,index1) 
        end do
        mat_val = mat_val_real / dim1 

        IF (my_rank == 0) THEN
            print *, "                Trace check: ", mat_val == true_val, true_val, mat_val
        END IF
    end subroutine check_trace 

    subroutine time_ring(type, calc_time, dim1, dim2)
        integer, intent(in) :: type
        real(wp), intent(out) :: calc_time
        integer, dim1, dim2
    
        ! Clock Start:
        call system_clock(count = count, count_rate = rate)
        timeAtStart = count / real(rate)

        ! Function selection:
        select case (type)
        case default
            print *, "No Case Selected! ERROR!"
        case (2) 
            call ring_cpu_slow(dim1,dim2,maxstep_ring)
        case (3)
            call ring_gpu_device(dim1,dim2,maxstep_ring)
        case (4)
            call ring_gpu_acc(dim1,dim2,maxstep_ring)
        case (5)
            call ring_cpu_fast(dim1,dim2,maxstep_ring)
        end select

        ! Clock Stop:
        call system_clock(count = count, count_rate = rate)
        timeAtEnd = count / real(rate)
        
        ! Calculate Time:
        calc_time = timeAtEnd-timeAtStart
        
    end subroutine time_ring

end program matrix_multi_test
