program derived
    implicit none

    ! Derived types are the "classes" of fortran. We can define them as follows:
    type animal
        character (len=20) :: name
        character (len=20) :: breed
        integer            :: age
    end type animal

    ! Initiations ---------------------------------------------------------------
    ! To initiate an instance we use the following:
    type(animal) :: x1, x2
    ! You can also create an array of a derived type:
    type(animal):: y(2)
    !----------------------------------------------------------------------------

    ! LoopStuff:
    integer :: i

    ! PROGRAM ===================================================================

    ! To put information in there, we use the following:
    x1%name  = "John"
    x1%breed = "Golden Retriever"
    x1%age   = 2

    write(*,*) x1%name, x1%breed, x1%age

    ! Alternatively we can assign the calues as follows:
    x2 = animal("Jonathan", "Silver Retriever", 3)
    write(*,*) x2%name, x2%breed, x2%age

    ! Filling the array:
    y(1) = x1
    y(2) = x2

    ! Write out the array:
    do i=1,2
        write(*,*) y(i)%name, y(i)%breed, y(i)%age
    end do


end program derived