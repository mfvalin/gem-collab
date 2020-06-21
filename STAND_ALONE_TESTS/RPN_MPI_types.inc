!/! RPN_MPI - Library of useful routines for C and FORTRAN programming
! ! Copyright (C) 1975-2020  Division de Recherche en Prevision Numerique
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
! public object types used for user application interface
!
! all these objects must be considered OPAQUE, their contents are managed 
! by the appropriate RPN_MPI_... routines and MUST NOT be altered by the user
!
  integer, parameter :: RPNMPI_types_version = 1

  type, bind(C)    :: rpnmpi_context
    type(c_ptr)    :: p     ! managed by rpn_mpi routines
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_context
  type(rpnmpi_context), parameter :: NULL_rpnmpi_context = rpnmpi_context(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_window
    type(c_ptr)    :: p     ! managed by rpn_mpi routines
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_window
  type(rpnmpi_window), parameter :: NULL_rpnmpi_window = rpnmpi_window(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_communicator
    type(c_ptr)    :: p     ! managed by rpn_mpi routines
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_communicator
  type(rpnmpi_communicator), parameter :: NULL_rpnmpi_communicator = rpnmpi_communicator(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_info
    type(c_ptr)    :: p     ! managed by rpn_mpi routines
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_info
  type(rpnmpi_info), parameter :: NULL_rpnmpi_info = rpnmpi_info(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_file
    type(c_ptr)    :: p     ! managed by rpn_mpi routines
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_file
  type(rpnmpi_file), parameter :: NULL_rpnmpi_file = rpnmpi_file(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_request
    type(c_ptr)    :: p     ! managed by rpn_mpi routines
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_request
  type(rpnmpi_request), parameter :: NULL_rpnmpi_request = rpnmpi_request(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_datatype
    type(c_ptr)    :: p     ! managed by rpn_mpi routines
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_datatype
  type(rpnmpi_datatype), parameter :: NULL_rpnmpi_datatype = rpnmpi_datatype(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_operator
    type(c_ptr)    :: p     ! managed by rpn_mpi routines
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_operator
  type(rpnmpi_operator), parameter :: NULL_rpnmpi_operator = rpnmpi_operator(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_group
    type(c_ptr)    :: p     ! managed by rpn_mpi routines
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_group
  type(rpnmpi_group), parameter :: NULL_rpnmpi_group = rpnmpi_group(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_ptr
    type(c_ptr)    :: p     ! managed by rpn_mpi routines
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_ptr
  type(rpnmpi_ptr), parameter :: NULL_rpnmpi_ptr = rpnmpi_ptr(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_pattern
    type(c_ptr)    :: p     ! pointer to data description
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_pattern
  type(rpnmpi_pattern), parameter :: NULL_rpnmpi_pattern = rpnmpi_pattern(C_NULL_PTR,0,0)

  type, bind(C)    :: rpnmpi_array
    type(c_ptr)    :: p     ! pointer to data description
    integer(c_int) :: t1    ! tag1 (reserved for rpn_mpi usage)
    integer(c_int) :: t2    ! tag2 (reserved for rpn_mpi usage)
  end type rpnmpi_array
  type(rpnmpi_array), parameter :: NULL_rpnmpi_array = rpnmpi_array(C_NULL_PTR,0,0)
!
! add interfaces to fortran <-> c conversion routines
!
include 'RPN_MPI_ftoc.inc'

