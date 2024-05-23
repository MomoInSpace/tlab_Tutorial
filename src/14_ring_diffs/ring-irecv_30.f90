PROGRAM ring

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the High Performance           !
! Computing Centre Stuttgart (HLRS).                           !
! The examples are based on the examples in the MPI course of  !
! the Edinburgh Parallel Computing Centre (EPCC).              !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! HLRS and EPCC take no responsibility for the use of the      !
! enclosed teaching material.                                  !
!                                                              !
! Authors: Joel Malard, Alan Simpson,            (EPCC)        !
!          Rolf Rabenseifner, Traugott Streicher (HLRS)        !
!                                                              !
! Contact: rabenseifner@hlrs.de                                !
!                                                              !
! Purpose: A program to try MPI_Issend and MPI_Recv.           !
!                                                              !
! Contents: F-Source                                           !
!                                                              !
!==============================================================!

  USE mpi_f08
  IMPLICIT NONE

  INTEGER :: my_rank, size
  INTEGER :: right, left
  INTEGER :: i, sum
  INTEGER, ASYNCHRONOUS :: snd_buf
  INTEGER :: rcv_buf
  TYPE(MPI_Status)  :: status
  TYPE(MPI_Request) :: request

  CALL MPI_Init()
  CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
  CALL MPI_Comm_size(MPI_COMM_WORLD, size)
  right = mod(my_rank+1,      size)
  left  = mod(my_rank-1+size, size)

  sum = 0
  snd_buf = my_rank
  DO i = 1, size
     CALL MPI_Irecv  (rcv_buf, 1, MPI_INTEGER, left,  17, MPI_COMM_WORLD, request)
     CALL MPI_Ssend(snd_buf, 1, MPI_INTEGER, right, 17, MPI_COMM_WORLD)
     CALL MPI_Wait(request, status)
     IF (.NOT.MPI_ASYNC_PROTECTS_NONBLOCKING) CALL MPI_F_sync_reg(rcv_buf)
     snd_buf = rcv_buf
     sum = sum + rcv_buf
  END DO
  WRITE(*,*) "PE", my_rank, ": Sum =", sum

  CALL MPI_Finalize()

END PROGRAM
