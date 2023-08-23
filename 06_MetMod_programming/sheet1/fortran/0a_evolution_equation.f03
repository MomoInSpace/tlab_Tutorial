program evol_equ_analytic
    use kind_parameter
    implicit none
    integer(i8),parameter:: steps = 1000
    integer(i8):: n_value
    ! real(dp)::
    real(dp),parameter:: delta_t=1
    real(dp):: a = 0.05, F = 1, T0 = 1
    real(dp):: start_t = 0, stop_t = 200*delta_t, time_value
    real(dp), dimension(0:steps):: time_array, value_array
    real(dp) T_analytic


    ! Integrating pi_func numerically
    do n_value = 0, steps
        time_value = stop_t*(real(n_value)/real(steps))
        time_array(n_value)  = time_value
        value_array(n_value) = T_analytic(time_value,a,F,-F/a)
        end do
    ! write(*,*) value_array
    call export_values(steps,time_array,value_array)


    end program evol_equ_analytic

subroutine export_values(steps,time_array,value_array)
    use kind_parameter
    implicit none
    integer(i8):: steps, i
    real(dp), dimension(steps):: time_array, value_array
    real(dp), dimension(2):: out_array
    open(unit=2,file='output_0a.csv',status='replace')

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

function T_analytic(t,a,F,T0)
    use kind_parameter
    implicit none
    real(dp):: t,a,F,T0,T_analytic
    T_analytic = F/a + T0*exp(-a*t)
    end function T_analytic
