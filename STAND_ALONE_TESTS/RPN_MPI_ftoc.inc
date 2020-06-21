!/! RPN_MPI - Library of useful routines for C and FORTRAN programming
! ! Copyright (C) 1975-2015  Division de Recherche en Prevision Numerique
! !                          Environnement Canada
! !
! ! This library is free software; you can redistribute it and/or
! ! modify it under the terms of the GNU Lesser General Public
! ! License as published by the Free Software Foundation,
! ! version 2.1 of the License.
! !
! ! This library is distributed in the hope that it will be useful,
! ! but WITHOUT ANY WARRANTY; without even the implied warranty of
! ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! ! Lesser General Public License for more details.
! !
! ! You should have received a copy of the GNU Lesser General Public
! ! License along with this library; if not, write to the
! ! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! ! Boston, MA 02111-1307, USA.
! !/
!
! interface to the routines used to convert between Fortran and C 
! communicators, data types, operators, ....
!
interface RPN_MPI_c2f
  function type_c2f(c) result(ftn) bind(C,name='RPN_MPI_type_c2f')
    import :: rpnmpi_datatype, C_INT
    implicit none
    type(rpnmpi_datatype), intent(IN) :: c
    integer(C_INT) :: ftn
  end function type_c2f

  function group_c2f(c) result(ftn) bind(C,name='RPN_MPI_group_c2f')
    import :: rpnmpi_group, C_INT
    implicit none
    type(rpnmpi_group), intent(IN) :: c
    integer(C_INT) :: ftn
  end function group_c2f

  function win_c2f(c) result(ftn) bind(C,name='RPN_MPI_window_c2f')
    import :: rpnmpi_window, C_INT
    implicit none
    type(rpnmpi_window), intent(IN) :: c
    integer(C_INT) :: ftn
  end function win_c2f

  function op_c2f(c) result(ftn) bind(C,name='RPN_MPI_op_c2f')
    import :: rpnmpi_operator, C_INT
    implicit none
    type(rpnmpi_operator), intent(IN) :: c
    integer(C_INT) :: ftn
  end function op_c2f

  function file_c2f(c) result(ftn) bind(C,name='RPN_MPI_file_c2f')
    import :: rpnmpi_file, C_INT
    implicit none
    type(rpnmpi_file), intent(IN) :: c
    integer(C_INT) :: ftn
  end function file_c2f

  function comm_c2f(c) result(ftn) bind(C,name='RPN_MPI_comm_c2f')
    import :: rpnmpi_communicator, C_INT
    implicit none
    type(rpnmpi_communicator), intent(IN) :: c
    integer(C_INT) :: ftn
  end function comm_c2f

  function req_c2f(c) result(ftn) bind(C,name='RPN_MPI_request_c2f')
    import :: rpnmpi_request, C_INT
    implicit none
    type(rpnmpi_request), intent(IN) :: c
    integer(C_INT) :: ftn
  end function req_c2f

  function info_c2f(c) result(ftn) bind(C,name='RPN_MPI_info_c2f')
    import :: rpnmpi_info, C_INT
    implicit none
    type(rpnmpi_info), intent(IN) :: c
    integer(C_INT) :: ftn
  end function info_c2f
end interface

interface RPN_MPI_f2c
  function type_f2c(ftn,mpi_fval) result(c) bind(C,name='RPN_MPI_type_f2c')
    import :: C_PTR, rpnmpi_datatype, C_INT
    implicit none
    type(rpnmpi_datatype), intent(INOUT) :: ftn
    integer(C_INT), intent(IN), value :: mpi_fval
    type(C_PTR) :: c
  end function type_f2c

  function group_f2c(ftn,mpi_fval) result(c) bind(C,name='RPN_MPI_group_f2c')
    import :: C_PTR, rpnmpi_group, C_INT
    implicit none
    type(rpnmpi_group), intent(INOUT) :: ftn
    integer(C_INT), intent(IN), value :: mpi_fval
    type(C_PTR) :: c
  end function group_f2c

  function win_f2c(ftn,mpi_fval) result(c) bind(C,name='RPN_MPI_window_f2c')
    import :: C_PTR, rpnmpi_window, C_INT
    implicit none
    type(rpnmpi_window), intent(INOUT) :: ftn
    integer(C_INT), intent(IN), value :: mpi_fval
    type(C_PTR) :: c
  end function win_f2c

  function op_f2c(ftn,mpi_fval) result(c) bind(C,name='RPN_MPI_op_f2c')
    import :: C_PTR, rpnmpi_operator, C_INT
    implicit none
    type(rpnmpi_operator), intent(INOUT) :: ftn
    integer(C_INT), intent(IN), value :: mpi_fval
    type(C_PTR) :: c
  end function op_f2c

  function file_f2c(ftn,mpi_fval) result(c) bind(C,name='RPN_MPI_file_f2c')
    import :: C_PTR, rpnmpi_file, C_INT
    implicit none
    type(rpnmpi_file), intent(INOUT) :: ftn
    integer(C_INT), intent(IN), value :: mpi_fval
    type(C_PTR) :: c
  end function file_f2c

  function comm_f2c(ftn,mpi_fval) result(c) bind(C,name='RPN_MPI_comm_f2c')
    import :: C_PTR, rpnmpi_communicator, C_INT
    implicit none
    type(rpnmpi_communicator), intent(INOUT) :: ftn
    integer(C_INT), intent(IN), value :: mpi_fval
    type(C_PTR) :: c
  end function comm_f2c

  function req_f2c(ftn,mpi_fval) result(c) bind(C,name='RPN_MPI_request_f2c')
    import :: C_PTR, rpnmpi_request, C_INT
    implicit none
    type(rpnmpi_request), intent(INOUT) :: ftn
    integer(C_INT), intent(IN), value :: mpi_fval
    type(C_PTR) :: c
  end function req_f2c

  function info_f2c(ftn,mpi_fval) result(c) bind(C,name='RPN_MPI_info_f2c')
    import :: C_PTR, rpnmpi_info, C_INT
    implicit none
    type(rpnmpi_info), intent(INOUT) :: ftn
    integer(C_INT), intent(IN), value :: mpi_fval
    type(C_PTR) :: c
  end function info_f2c
end interface

