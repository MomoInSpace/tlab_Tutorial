program mpi_scatter_example
    use mpi
    implicit none

    integer:: ierr, rank, size
    integer, dimension(:), allocatable:: sendbuf, recvbuf
    integer:: n, i

    ! Initialize the MPI environment
    call MPI_Init(ierr)

    ! Get the rank of the process
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    
    ! Get the total number of processes
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

    ! Assume we want to scatter an array of size 4
    n = 4

    ! The send buffer is only allocated by the root process (rank 0)
    if (rank == 0) then
        allocate(sendbuf(n))
        ! Fill the send buffer with data
        do i = 1, n
            sendbuf(i) = i
        end do
    end if

    ! Each process allocates a receive buffer to hold its portion of data
    allocate(recvbuf(1))

    ! Scatter the data from process 0 to all processes
    call MPI_Scatter(sendbuf, 1, MPI_INTEGER, recvbuf, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

    ! Each process outputs its received data
    print *, 'Process ', rank, ' received ', recvbuf(1)

    ! Deallocate buffers
    if (rank == 0) then
        deallocate(sendbuf)
    end if
    deallocate(recvbuf)

    ! Finalize the MPI environment
    call MPI_Finalize(ierr)

end program mpi_scatter_example
