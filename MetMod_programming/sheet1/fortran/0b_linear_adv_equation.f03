program lin_adv_equation 
    use kind_parameter
    implicit none
    
    ! Function T(x,t) parameters analytic:---------------------
    real(dp) T_analytic
    complex(dp), parameter :: I = (0,1)

    real(dp) :: x ! Space variable -> Discretisized with delta_x
    real(dp) :: t ! Time variable -> Discretisized with delta_t 
    real(dp) :: u   = 10  ! phase velocity
    real(dp) :: T_1 = 10  ! Amplitude of Wave
    real(dp) :: k   = 1   ! Wavenumber
    ! ---------------------------------------------------------

    ! Discretisation Parameters:-------------------------------
    integer(i8), parameter :: NX = 100     ! Gridpoints in space
    integer(i8), parameter :: N_time = 100 ! Gridpoints in time 
    
    real(dp), parameter :: delta_x = 1 ! Gridspacing
    real(dp), parameter :: delta_t = 1 ! Timespacing

    integer(i8) :: time_step = 0
    real(dp), dimension(0:N_time):: time_array, value_array
    ! ---------------------------------------------------------

    do time_step = 0, N_time
        t = delta_t*real(time_step)/real(N_time)
        time_array(time_step)  = t 
        value_array(time_step) = T_analytic(t,T_1,k,x,u)
        end do
    ! write(*,*) value_array
    call export_values('output_0b.csv', &
                       time_step,  &
                       time_array, &
                       value_array)


    end program lin_adv_equation 

subroutine export_values(output_file, steps,time_array,value_array)
    use kind_parameter
    implicit none
    character :: output_file
    integer(i8):: steps, i
    real(dp), dimension(steps):: time_array, value_array
    real(dp), dimension(2):: out_array
    open(unit=2,file=output_file,status='replace')

    write(2, '(A)') "time,values"
    do i = 1, steps
        out_array(1) = time_array(i)
        out_array(2) = value_array(i)
        write(2,"(*(G0,:,','))") out_array
        ! write(2,'(2F3.3)') out_array
        ! write(2,*) out_array
    end do
    ! write(2,*) "time_array, value_array"
    ! write(2,1F10.3) time_array[i]
    ! write(2,2F10.3) value_array[i]
    ! write(2,2F10.3) out_array

    close(2)
    end subroutine

function T_analytic(t,T_1,k,x,u)
    use kind_parameter
    implicit none
    real(dp):: t,T_1,k,x,u,T_analytic
    complex(dp) :: I = (0,1)
    T_analytic = Real(T_1*exp(I*k*(x-u*t)))
    end function T_analytic
