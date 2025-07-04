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

    subroutine matmul_cpu_slow(x_mat, y_mat, z_mat, Mnum)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        integer :: n, m, k, i, j, Mnum, iMnum

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix Multiplication:
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        do iMnum = 1, Mnum
            do j=1,n
                do i=1,m
                    do k=1,size(x_mat,dim=2)
                        z_mat(i,j)=z_mat(i,j)+ x_mat(i,k)*y_mat(k,j)
                    end do      
                end do
            end do
        end do

    end subroutine matmul_cpu_slow

    subroutine matmul_gpu_acc_kernels(x_mat, y_mat, z_mat, Mnum)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        real(wp) :: sum_value
        integer :: n, m, k, i, j, Mnum, iMnum

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix Multiplication:
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        !$acc data copyin(x_mat,y_mat) copyout(z_mat)
        do iMnum = 1, Mnum
        !$acc kernels
        do j=1,n
            do i=1,m
                sum_value = 0
                do k=1,size(x_mat,dim=2)
                    sum_value = sum_value + x_mat(i,k)*y_mat(k,j)
                end do      
                z_mat(i,j)= sum_value 
            end do
        end do
        !$acc end kernels
        end do
        !$acc end data 

    end subroutine matmul_gpu_acc_kernels

    subroutine matmul_gpu_acc_kernels_noCopy(x_mat, y_mat, z_mat, Mnum)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        real(wp) :: sum_value
        integer :: n, m, k, i, j, Mnum, iMnum

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix Multiplication:
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        !$acc data
        do iMnum = 1, Mnum
        !$acc kernels
        do j=1,n
            do i=1,m
                sum_value = 0
                do k=1,size(x_mat,dim=2)
                    sum_value = sum_value + x_mat(i,k)*y_mat(k,j)
                end do      
                z_mat(i,j)= sum_value 
            end do
        end do
        !$acc end kernels
        end do
        !$acc end data 

    end subroutine matmul_gpu_acc_kernels_noCopy

    subroutine matmul_gpu_acc_kernels_noData(x_mat, y_mat, z_mat, Mnum)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        real(wp) :: sum_value
        integer :: n, m, k, i, j, Mnum, iMnum

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix Multiplication:
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        do iMnum = 1, Mnum
        !$acc kernels copyout(z_mat,x_mat,y_mat) copyin(z_mat,x_mat,y_mat)
        do j=1,n
            do i=1,m
                sum_value = 0
                do k=1,size(x_mat,dim=2)
                    sum_value = sum_value + x_mat(i,k)*y_mat(k,j)
                end do      
                z_mat(i,j)= sum_value 
            end do
        end do
        !$acc end kernels
        end do

    end subroutine matmul_gpu_acc_kernels_noData

    subroutine matmul_gpu_acc_loop(x_mat, y_mat, z_mat, Mnum)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        integer :: n, m, k, i, j, Mnum, iMnum

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix Multiplication:
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        !$acc data copyin(x_mat,y_mat) copyout(z_mat)
        do iMnum = 1, Mnum
            !$acc parallel loop
            do j=1,n
                !$acc loop
                do i=1,m
                    do k=1,size(x_mat,dim=2)
                        z_mat(i,j)= z_mat(i,j)+x_mat(i,k)*y_mat(k,j)
                    end do      
                end do
                !No nested $acc end parallel loop
            end do
            !$acc end parallel loop
        end do
        !$acc end data

    end subroutine matmul_gpu_acc_loop

    subroutine matmul_omp(x_mat, y_mat, z_mat, Mnum)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        integer :: n, m, k, i, j, Mnum, iMnum

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix Multiplication:
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        do iMnum = 1, Mnum
        !$omp parallel do 
            do j=1,n
            !$omp parallel do 
                do i=1,m
                    do k=1,size(x_mat,dim=2)
                        z_mat(i,j)= z_mat(i,j)+x_mat(i,k)*y_mat(k,j)
                    end do      
                end do
                !No nested $acc end parallel loop
            end do
        end do

    end subroutine matmul_omp 

    subroutine matmul_cpu_dotmix(x_mat, y_mat, z_mat, Mnum)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        integer :: n, m, k, i, j, Mnum, iMnum

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix multiplication
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        do iMnum = 1, Mnum
            do j=1,n
                do i=1,m
                        z_mat(i,j)=dot_product(x_mat(i,:),y_mat(:,j))
                end do
            end do
        end do

    end subroutine matmul_cpu_dotmix

    subroutine matmul_gpu_dotmix(x_mat, y_mat, z_mat, Mnum)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        integer :: n, m, k, i, j, Mnum, iMnum

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.

        ! Matrix multiplication
        n=size(y_mat,dim=2)
        m=size(x_mat,dim=1)

        !$acc data copyin(x_mat,y_mat) copyout(z_mat)
        do iMnum = 1, Mnum
            !$acc parallel loop
            do j=1,n
                !$acc loop
                do i=1,m
                        z_mat(i,j)=dot_product(x_mat(i,:),y_mat(:,j))
                end do
                !lll $acc end parallel loop
            end do
            !$acc end parallel loop
        end do
        !$acc end data

    end subroutine matmul_gpu_dotmix

    subroutine matmul_cpu_intrinsic(x_mat, y_mat, z_mat, Mnum)
        real(wp), dimension(:,:),intent(in)  :: x_mat
        real(wp), dimension(:,:),intent(in)  :: y_mat
        real(wp), dimension(:,:),intent(out) :: z_mat
        integer :: Mnum, iMnum

        ! Check if shapes match:
        call check_shapes(x_mat, y_mat, z_mat)
        ! Set values in z_mat to zero:
        ! z_mat = 0.
        ! Matrix multiplication
        
        do iMnum = 1, Mnum
            z_mat = matmul(x_mat,y_mat)
        end do

    end subroutine matmul_cpu_intrinsic

    ! Not Supported:
    ! subroutine matmul_gpu_intrinsic(x_mat, y_mat, z_mat)
    !     real(wp), dimension(:,:),intent(in)  :: x_mat
    !     real(wp), dimension(:,:),intent(in)  :: y_mat
    !     real(wp), dimension(:,:),intent(out) :: z_mat

    !     ! Check if shapes match:
    !     call check_shapes(x_mat, y_mat, z_mat)
    !     ! Set values in z_mat to zero:
    !     ! z_mat = 0.
    !     ! Matrix multiplication

    !     !$acc kernels
    !     z_mat = matmul(x_mat,y_mat)
    !     !$acc end kernels

    ! end subroutine matmul_gpu_intrinsic

end module
