! In this program, we want to show

program matrix_multi_test
    use TLAB_ARRAYS 
    use TLAB_CONSTANTS, only: wp
    implicit none 
    integer :: x_dim = 3, y_dim = 3
    
    allocate(x(x_dim,y_dim))
    allocate(y(x_dim,y_dim))
    x = reshape([1,2,3,0,4,0,0,0,5],[x_dim,y_dim])

    
    ! case 4
    ! does x have to have at least 2 dims in each direction?

    y = x ! No allocation. Works as pointer? Or just somewhere in the 
    z = matmul(x,y)
    call write_out_matrixform(z)

    ! deallocate(x)
    ! deallocate(y)
    
    
     
end program matrix_multi_test

subroutine write_out_matrixform(matrix)
    use TLAB_ARRAYS
    use TLAB_CONSTANTS, only: wp
    implicit none 
    real(wp), intent(in), dimension (3,3) :: matrix
    integer :: i,j
    integer, parameter :: n = 3

    ! print *, matrix(1,1) 
    ! write(*,'(F10.3)')  matrix 
    ! write(*,'(g0)')  matrix 
    write( * , "(*(g0.3))" ) ( (matrix(i,j)," ",j=1,n), new_line("A"), i=1,n )
    ! write(*, *)  matrix 
    ! print *, matrix(:,2) 
    ! print *, matrix(:,3) 


    
end subroutine write_out_matrixform