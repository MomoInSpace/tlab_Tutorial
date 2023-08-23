module MATRIX_MULTIPLY_MOD 
    use TLAB_CONSTANTS, only: wp
    implicit none

contains

    subroutine check_shapes(x_mat, y_mat, z_mat)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(in)  :: z_mat
    
        ! Check size of input arrays
        if(size(x_mat,dim=2) /= size(y_mat,dim=1)) stop "x and y input array size not match"

        ! Check if z_mat has the right size
        if(size(x_mat,dim=1) /= size(z_mat,dim=1)) stop "x and z input array size not match"
        if(size(y_mat,dim=2) /= size(z_mat,dim=2)) stop "y and z input array size not match"
        
    end subroutine check_shapes

    subroutine matmul_cpu_slow(x_mat, y_mat, z_mat)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        integer :: n, m, k, i, j

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix Multiplication:
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        do j=1,n
            do k=1,size(x_mat,dim=2)
                do i=1,m
                    z_mat(i,j)=z_mat(i,j) + x_mat(i,k)*y_mat(k,j)
                end do      
            end do
        end do

    end subroutine matmul_cpu_slow

    subroutine matmul_cpu_acc_kernels(x_mat, y_mat, z_mat)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        integer :: n, m, k, i, j

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix Multiplication:
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        !$acc kernels
        do j=1,n
            do k=1,size(x_mat,dim=2)
                do i=1,m
                    z_mat(i,j)=z_mat(i,j) + x_mat(i,k)*y_mat(k,j)
                end do      
            end do
        end do
        !$acc end kernels

    end subroutine matmul_cpu_acc_kernels

    subroutine matmul_cpu_acc_loop(x_mat, y_mat, z_mat)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        integer :: n, m, k, i, j

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix Multiplication:
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        !$acc parallel loop
        do j=1,n
            !asdfasdfafdacc parallel loop
            do k=1,size(x_mat,dim=2)
                do i=1,m
                    z_mat(i,j)=z_mat(i,j) + x_mat(i,k)*y_mat(k,j)
                end do      
            end do
        end do
        !$acc end parallel loop

    end subroutine matmul_cpu_acc_loop

    subroutine matmul_cpu_dotmix(x_mat, y_mat, z_mat)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        integer :: n, m, i, j

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix multiplication
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        do j=1,n
            do i=1,m
                    z_mat(i,j)=dot_product(x_mat(i,:),y_mat(:,j))
            end do
        end do

    end subroutine matmul_cpu_dotmix

    subroutine matmul_cpu_intrinsic(x_mat, y_mat, z_mat)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.
        ! Matrix multiplication
        z_mat = matmul(x_mat,y_mat)

    end subroutine matmul_cpu_intrinsic

end module
