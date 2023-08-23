module MATRIX_OUTPUT
    implicit none

    contains

    subroutine write_out_matrixform(matrix)
        use TLAB_CONSTANTS, only: wp
        implicit none 
        integer :: i,j
        integer, dimension(:,:),intent(in)  :: matrix
        integer, parameter :: n = 2

        ! Code: ===============================================================

        write( * , "(*(g0.3))" ) ( (matrix(i,j)," ",j=1,n), new_line("A"), i=1,n )
        
    end subroutine write_out_matrixform

end module MATRIX_OUTPUT