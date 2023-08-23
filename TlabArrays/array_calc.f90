program matrix_multi_test
    use TLAB_CONSTANTS, only: wp
    implicit none 

    ! We create a matrix by allocating some space on the disk:
    integer,target :: mat(3,3)

    ! Now we can create a pointer to work on a submatrix later on:
    integer,pointer :: p(:,:)

    ! Alternatively, we can use allocatables:
    integer, allocatable :: mat_alloc(:,:)
    
    ! Code: ==========================================================

    ! Working with pointers: -------------------------------------------
    ! Here we set the variables in the matrix mat:
    mat = reshape([1,2,3,0,4,0,0,0,5],shape(mat))

    ! Because we just want to work on a sumatrix, we set our pointer 
    ! to said submatrix:
    p => mat(1:2,1:2)
    ! Here we output p in matrixform in the terminal:
    call write_out_matrixform(p)

    ! Now we want to look at a different part of the matrix:
    p => mat(2:3,2:3)
    call write_out_matrixform(p)

    ! Because we don't want to use the pointer anymore, it is safest 
    ! to let it point to null()
    p => null()


    ! Working with allocatables: -----------------------------------------
    ! We can use the allocatable similarly to a pointer:
    mat_alloc = mat(1:2,1:2)
    call write_out_matrixform(mat_alloc)

    ! Though we could also just allocate a new matrix with the allocatable:
    deallocate(mat_alloc)
    allocate(mat_alloc(2,2))
    ! We would have to fill it with values, then we can print it:
    mat_alloc = reshape([1,1,1,1],[2,2])
    call write_out_matrixform(mat_alloc)

    ! The matrix mat is obviously still allocated, just mat_aloc 
    ! doesn't point there anymore:
    p => mat(1:2,1:2)
    call write_out_matrixform(p)
    p => null()

end program matrix_multi_test

subroutine write_out_matrixform(matrix)
    use TLAB_ARRAYS
    use TLAB_CONSTANTS, only: wp
    implicit none 
    integer, intent(in), dimension (2,2) :: matrix
    integer :: i,j
    integer, parameter :: n = 2

    ! Code: ===============================================================

    write( * , "(*(g0.3))" ) ( (matrix(i,j)," ",j=1,n), new_line("A"), i=1,n )
    
end subroutine write_out_matrixform