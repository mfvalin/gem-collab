! RPN_COMM - Library of useful routines for C and FORTRAN programming
! Copyright (C) 2020  Division de Recherche en Prevision Numerique
!                     Environnement Canada
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
program test_shared_window
  use ISO_C_Binding
  implicit none
  integer :: siz, rank, ierr, sockcomm, nodecomm, peercomm, noderank, sockrank, peerrank, isiz, win, msize, i, errors
  type(C_PTR) :: baseptr
  integer, dimension(:), pointer :: array
  include 'mpif.h'
  include 'RPN_COMM.inc'

  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD,siz ,ierr)
  call mpi_comm_rank(MPI_COMM_WORLD,rank,ierr)
  call init_logical_cpu_configuration
  if(rank == 0) call print_logical_cpu_configuration
  call RPN_COMM_split_by_socket(MPI_COMM_WORLD, nodecomm, sockcomm, peercomm, noderank, sockrank, peerrank, isiz, ierr)
  msize = 1024*1024
  call RPN_COMM_win_allocate_shared(sockcomm, msize, win, baseptr, ierr)
  call c_f_pointer(baseptr, array, [msize])
  if(sockrank == 0) then
    print *, 'isiz =',isiz
    do i = 1, msize
      array(i) = i
    enddo
  endif
  call mpi_barrier(sockcomm,ierr)
  errors = 0
  do i = 1, msize
    if(array(i) .ne. i) errors = errors + 1
  enddo
  print *,'PE, sockrank, errors =',rank, sockrank, errors
  call mpi_finalize(ierr)

end program
