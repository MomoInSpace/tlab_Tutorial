program matrix_multi_test
    use TLAB_ARRAYS
    use TLAB_CONSTANTS, only: wp
    implicit none 
    integer,target :: mat(3,3)
    ! integer,pointer :: p(:,:)
    integer :: x_dim = 3, y_dim = 3
    
    mat = reshape([1,2,3,0,4,0,0,0,5],shape(mat))
    allocate(x(x_dim,y_dim))
    allocate(y(x_dim,y_dim))
    x = reshape([1,2,3,0,4,0,0,0,5],[x_dim,y_dim])

    
    ! case 4
    ! does x have to have at least 2 dims in each direction?

    ! x = mat
    ! y = mat
    ! z = matmul(x,y)
    z = mat
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