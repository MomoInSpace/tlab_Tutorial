module basis_module
  implicit none

  type, abstract :: Basis
    integer :: NBasis
  contains
    procedure(allocBasisR1Interface), deferred :: allocateBasisR1
    generic :: allocateBasis => allocateBasisR1
  end type Basis

  interface 
    ! Interface for real basis allocation
    subroutine allocBasisR1Interface(self, array)
      import
      class(Basis), intent(inout) :: self
      real, intent(in) :: array(:)
    end subroutine allocBasisR1Interface
  end interface

end module basis_module


module extension_module
  use basis_module
  implicit none

  type, extends(Basis) :: GridBasis
  contains
    ! Extending the mapping allocateBasis => allocateBasisR1 of
    ! the parent type.
    generic :: allocateBasis => allocateBasisC1
    procedure :: allocateBasisC1
    ! Implementation for the deferred procedure in Basis
    procedure :: allocateBasisR1
  end type GridBasis

contains

  subroutine allocateBasisR1(self, array)
    class(GridBasis), intent(inout) :: self
    real, intent(in) :: array(:)

    self%NBasis = size(array)
    print *, "GridBasis:allocateBasisR1"

  end subroutine allocateBasisR1


  subroutine allocateBasisC1(self, array)
    class(GridBasis), intent(inout) :: self
    complex, intent(in) :: array(:)

    self%NBasis = size(array)
    print *, "GridBasis:allocateBasisC1"

  end subroutine allocateBasisC1

end module extension_module


program test
  use extension_module
  implicit none

  type(GridBasis) :: mybasis
  real :: myRealArray(10)
  complex :: myComplexArray(5)

  call mybasis%allocateBasis(myRealArray)
  call mybasis%allocateBasis(myComplexArray)

end program test
