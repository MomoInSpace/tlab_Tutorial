module mpi_utils
    implicit none
    contains

    subroutine gather_and_print_characters(my_chars, MPI_print_comm)

        ! Arguments
        character(len=*), intent(in):: my_chars   ! Each process's character array

        ! Locals
        integer:: ierr, my_rank, num_procs, root, i
        character(len = 30000):: gathered_chars   ! Character array to hold gathered results
        TYPE(MPI_Comm):: MPI_print_comm

        call MPI_COMM_RANK(MPI_print_comm, my_rank, ierr)
        call MPI_COMM_SIZE(MPI_print_comm, num_procs, ierr)

        root = 0

        ! Gather all character arrays at the root
        call MPI_GATHER(my_chars, len(my_chars), MPI_CHARACTER, &
                        gathered_chars, len(my_chars), MPI_CHARACTER, root, MPI_print_comm, ierr)

        ! Root process prints the gathered characters in order
        if (my_rank == root) then
            print*, "Gathered characters in order:"
            do i = 1, num_procs
                ! write(*,*) "noice"
                write(*,*,advance = 'no') trim(gathered_chars((i-1)*len(my_chars)+1:i*len(my_chars)))
            end do
            ! deallocate(gathered_chars)  ! Deallocate dynamically allocated array
        end if

    end subroutine gather_and_print_characters
end module mpi_utils
