program evol_equ_analytic
    use kind_parameter
    use parameters
    use EXPORT_ARRAYS
    implicit none
    
    ! Parameters of Mountain:
    real(dp), parameter :: Lxb = 1000
    real(dp), parameter :: Lzb = 300

    ! Parameters delta(x,z)
    real(dp), parameter :: U = 10
    real(dp), parameter :: g = 9.81
    real(dp), parameter :: theta0 = 290
    real(dp), parameter :: N = sqrt(g/theta0 * 0.005)

    ! Stromlinienverschiebung
    real(dp), allocatable :: zs(:)
    real(dp), allocatable :: delta_xz(:,:)
    real(dp), allocatable :: theta_xz(:,:)


    ! write(*,*) Lxb, Lzb, U, g, theta0, N
    

    call init_grid()
    
    allocate(zs(steps_x))
    allocate(delta_xz(steps_x,steps_z))
    allocate(theta_xz(steps_x,steps_z))

    do n_value = 1, steps_x
        zs(n_value) = zs_mountain_height(x(n_value))

        do k_value = 1, steps_z
            delta_xz(n_value,k_value) = &
                delta_stromlinienverschiebung(x(n_value), &
                                            zs(n_value),&
                                            z(k_value))
            theta_xz(n_value,k_value) = & 
                theta_func(delta_xz(n_value,k_value),z(k_value))         
        end do
    end do

    ! call write_out_matrixform(delta_xz)

    call save_array(x,"data/x.csv")
    call save_array(z,"data/z.csv")
    call save_array(zs,"data/zs.csv")
    call save_matrix(delta_xz,"data/delta.csv")
    call save_matrix(theta_xz,"data/theta.csv")
    ! call write_out_matrixform(zs)


    ! Integrating pi_func numerically
    ! do n_value = 0, steps
    !     time_value = stop_t*(real(n_value)/real(steps))
    !     time_array(n_value)  = time_value
    !     value_array(n_value) = T_analytic(time_value,a,F,-F/a)
    !     end do
    ! write(*,*) value_array
    ! call export_values(steps,time_array,value_array)

        contains 

        ! function zs_mountain_height()

        function f2(x_val, zs_val)
            use kind_parameter
            use parameters
            implicit none
            real(dp) :: x_val, zs_val, f2
            f2 = - x_val/Lxb*zs_val
            end function f2 

        function zs_mountain_height(x_val)
            use kind_parameter
            use parameters
            implicit none
            real(dp) :: x_val, zs_mountain_height
            zs_mountain_height = Lzb*Lxb**2/(Lxb**2+x_val**2) 
            end function zs_mountain_height

        function delta_stromlinienverschiebung(x_val, zs_val, z_val)
            use kind_parameter
            use parameters
            implicit none
            real(dp) :: x_val, zs_val, z_val, &
                        delta_stromlinienverschiebung
            delta_stromlinienverschiebung = &
                zs_val*cos(N/U*(z_val-zs_val))+ f2(x_val,zs_val)*sin(N/U*(z_val-zs_val))
            end function delta_stromlinienverschiebung

        function theta_func(delta_val, z_val)
            use kind_parameter
            use parameters
            implicit none
            real(dp) :: delta_val, z_val, &
                        theta_func
            theta_func = &
                theta0*(1+ N**2*(z_val-delta_val)/g)
            end function theta_func


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

! function T_analytic(t,a,F,T0)
!     use kind_parameter
!     implicit none
!     real(dp):: t,a,F,T0,T_analytic
!     T_analytic = F/a + T0*exp(-a*t)
!     end function T_analytic


