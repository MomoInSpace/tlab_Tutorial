module EXPORT_ARRAYS 
    implicit none
    
contains

    subroutine save_array(matrix,filename)
        use TLAB_CONSTANTS, only: wp
        ! Matrix with information:
        real(wp), dimension(:),intent(in)  :: matrix
        ! Looping over matrix with shape:
        integer :: i,j
        integer, dimension(1) :: my_shape
        integer :: n
        ! Saving in file:
        character(len=*), intent(in) :: filename

        ! Code: ===============================================================

        my_shape = shape(matrix)
        n = my_shape(1)

        open(unit=2,file=filename,status='replace')
        write( 2 , "(*(g0.5))" ) ( matrix(i), new_line("A"), i=1,n ) 
        close(2)

    end subroutine save_array 

    subroutine save_matrix(matrix,filename)
        use TLAB_CONSTANTS, only: wp
        ! Matrix with information:
        real(wp), dimension(:,:),intent(in)  :: matrix
        ! Looping over matrix with shape:
        integer :: i,j
        integer, dimension(2) :: my_shape
        integer :: n,k
        ! Saving in file:
        character(len=*), intent(in) :: filename

        ! Code: ===============================================================

        my_shape = shape(matrix)
        n = my_shape(1)
        k = my_shape(2)

        open(unit=2,file=filename,status='replace')
        write( 2 , "(*(g0.5))" ) ( (matrix(i,j),",",j=1,k), new_line("A"), i=1,n ) 
        close(2)

    end subroutine save_matrix 

    subroutine write_out_matrixform(matrix)
        use TLAB_CONSTANTS, only: wp
        integer :: i,j
        real(wp), dimension(:,:),intent(in)  :: matrix
        integer, dimension(2) :: my_shape
        integer :: n,k

        ! Code: ===============================================================

        my_shape = shape(matrix)
        n = my_shape(1)
        k = my_shape(2)
        write( * , "(*(g0.3))" ) ( (matrix(i,j)," ",j=1,n), new_line("A"), i=1,n ) 
        ! ! my_shape = shape(matrix)
        ! n = my_shape(1)
        ! write(*, *) n, "x",k
        ! n =  shape(matrix)(1)

        
    end subroutine write_out_matrixform   
        

end module EXPORT_ARRAYS 