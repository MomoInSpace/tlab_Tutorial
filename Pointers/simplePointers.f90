program pointerTest
    implicit none
    integer, target  :: config(5) = [1,2,3,4,5]

    ! pointer to single variable:
    integer, pointer :: number_of_records => config(2)
    ! pointer to subarray:
    integer, pointer :: p(:) => null()

    ! Use a pointer like a variable:
    print *, number_of_records

    ! Update:
    config(2) = 99
    print *, number_of_records

    ! First three Values:
    p => config(1:3)
    print *, p

    p => config(3:5)
    print *,p

    


end program pointerTest