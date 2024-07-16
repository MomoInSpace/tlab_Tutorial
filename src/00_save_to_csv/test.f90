PROGRAM test
    use TLAB_CONSTANTS, only: wp
    implicit none
    integer :: i, index1, index2, index3
    real(wp), allocatable :: time(:,:)!(3,timesteps)
    integer     :: timesteps! = maxsize/step 


    timesteps = 3
    allocate(time(3,timesteps))
    time(1:3,1) = [1920230.,3929191992.,2392939299999.]
    call save_array(time(1:3,1),'TEEEEST.csv')

    
contains

    subroutine save_array(matrix,filename)
        use TLAB_CONSTANTS, only: wp
        ! Matrix with information:
        real(wp), dimension(:),intent(in)  :: matrix
        ! Looping over matrix with shape:
        integer :: i,j
        integer, dimension(1) :: my_shape
        integer :: n, io
        ! Saving in file:
        character(len=*), intent(in) :: filename

        ! Code: ===============================================================

        !my_shape = shape(matrix)
        !print(my_shape)
        n = size(matrix) !my_shape(1)

        open(newunit=io,file=filename,position='append', action='write')
        write(io , "(*(g10.5))" ) ( matrix(i), ", ", i=1,n ) , new_line("A")
        close(io)

    end subroutine save_array 

END PROGRAM
