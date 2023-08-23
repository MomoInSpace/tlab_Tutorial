module MATRIX_OUTPUT
    implicit none

    contains

    subroutine write_out_matrixform(matrix)
        use TLAB_CONSTANTS, only: wp
        integer :: i,j
        real(wp), dimension(:,:),intent(in)  :: matrix
        integer, dimension(2) :: my_shape
        integer :: n

        ! Code: ===============================================================

        my_shape = shape(matrix)
        n = my_shape(1)
        ! write(*, *) n
        ! n =  shape(matrix)(1)

        write( * , "(*(g0.3))" ) ( (matrix(i,j)," ",j=1,n), new_line("A"), i=1,n )
        
    end subroutine write_out_matrixform

end module MATRIX_OUTPUT