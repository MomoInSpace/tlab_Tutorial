! In this program we want to explain 
! - how to allocate a matrix
! - pointers
! - allocatable matrices

program matrix_multi_test
    use TLAB_CONSTANTS, only: wp
    use TLAB_ARRAYS 
    use MATRIX_OUTPUT
    implicit none 
    integer :: dim1 = 3, dim2 = 3
    integer :: index1, index2

    allocate(x(dim1,dim2))
    allocate(y(dim1,dim2))
    allocate(z(dim1,dim2))


    do index1 = 1, dim1
        do index2 = 1, dim2
            x(index1,index2) = 1
            if ( index1==index2 ) then
                y(index1,index2) = index1
            end if
        end do
    end do

    ! x = reshape([1,2,3,0,4,0,0,0,5],[dim1,dim2])
    ! y = reshape([1,2,3,0,4,0,0,0,5],[dim1,dim2])

    call write_out_matrixform(x)
    call write_out_matrixform(y)

    z = matmul(x,y)
    call write_out_matrixform(z)
    z = matmul(y,x)
    call write_out_matrixform(z)

end program matrix_multi_test