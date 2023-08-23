module EXPORT_ARRAYS 
    implicit none
    
contains

    subroutine export_array(matrix,filename)
        use kind_parameter
        implicit none
        integer :: i,j
        integer, dimension(:,:),intent(in)  :: matrix
        character, intent(in) :: filename(:)
        integer, dimension(2) :: my_shape
        integer :: n
        ! Old:
        ! integer(i8):: steps, i
        ! real(dp), dimension(steps):: time_array, value_array
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

    subroutine write_out_matrixform(matrix)
        use TLAB_CONSTANTS, only: wp
        integer :: i,j
        integer, dimension(:,:),intent(in)  :: matrix
        integer, dimension(2) :: my_shape
        integer :: n

        ! Code: ===============================================================

        my_shape = shape(matrix)
        n = my_shape(1)
        write(*, *) n
        ! n =  shape(matrix)(1)

        write( * , "(*(g0.3))" ) ( (matrix(i,j)," ",j=1,n), new_line("A"), i=1,n )
        
    end subroutine write_out_matrixform   
        

end module EXPORT_ARRAYS 