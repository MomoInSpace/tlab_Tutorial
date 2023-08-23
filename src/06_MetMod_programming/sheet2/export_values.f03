module EXPORT_ARRAYS 
    implicit none
    
contains

    ! subroutine export_array(matrix,filename)
    !     use kind_parameter
    !     implicit none
    !     integer :: i,j
    !     integer, dimension(:,:),intent(in)  :: matrix
    !     integer, dimension(2) :: my_shape
    !     integer :: n
    !     ! Old:
    !     ! integer(i8):: steps, i
    !     ! real(dp), dimension(steps):: time_array, value_array
    !     real(dp), dimension(2):: out_array
    !     open(unit=2,file='output_0a.csv',status='replace')

    !     write(2, '(A)') "time,values"
    !     do i = 1, steps
    !         out_array(1) = time_array(i)
    !         out_array(2) = value_array(i)
    !         write(2,"(*(G0,:,','))") out_array
    !         ! write(2,'(2F3.3)') out_array
    !         ! write(2,*) out_array
    !     end do
    !     ! write(2,*) "time_array, value_array"
    !     ! write(2,1F10.3) time_array[i]
    !     ! write(2,2F10.3) value_array[i]
    !     ! write(2,2F10.3) out_array

    !     close(2)
    !     end subroutine

    ! subroutine write_out_matrixform(matrix)
    !     use kind_parameter, only: dp
    !     integer :: i,j
    !     real(dp), dimension(:),intent(in)  :: matrix
    !     integer, dimension(:), allocatable :: my_shape
    !     integer :: n,k

    !     ! Code: ===============================================================
    !     do dim = 1, len(shape(matrix))
            
    !     end do

    !     if ( size(shape(matrix)) == 1 ) then
    !         allocate(my_shape(1))
    !         my_shape = shape(matrix)
    !         n = my_shape(1)
    !         ! k = my_shape(2)
    !         write( * , "(*(g0.3))" )  (matrix(j)," ",j=1,n) !, new_line("A"), i=1,n ) 
    !     end if
    !     if ( size(shape(matrix)) == 2 ) then
    !         allocate(my_shape(2))
    !         my_shape = shape(matrix)
    !         n = my_shape(1)
    !         k = my_shape(2)
    !         write( * , "(*(g0.3))" ) ( (matrix(i+j)," ",j=1,n), new_line("A"), i=1,n ) 
    !     end if
    !     ! ! my_shape = shape(matrix)
    !     ! n = my_shape(1)
    !     ! write(*, *) n, "x",k
    !     ! n =  shape(matrix)(1)

        
    ! end subroutine write_out_matrixform   
    subroutine save_array(matrix,filename)
        use kind_parameter, only: dp
        ! Matrix with information:
        real(dp), dimension(:),intent(in)  :: matrix
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
        use kind_parameter, only: dp
        ! Matrix with information:
        real(dp), dimension(:,:),intent(in)  :: matrix
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
        use kind_parameter, only: dp
        integer :: i,j
        real(dp), dimension(:,:),intent(in)  :: matrix
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