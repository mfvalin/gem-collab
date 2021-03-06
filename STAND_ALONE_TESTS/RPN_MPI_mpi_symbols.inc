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
!===================================================================
  integer, parameter :: mpi_symbols_version = 100012 ! version 1.00.01
!===================================================================
!
! RPN_MPI wrappers equivalent MPI wrapped derived types (used to enforce interfaces)
!
  type, BIND(C) :: RPN_MPI_Loc     ! wrapped generic array address
    integer(C_INTPTR_T) :: a
  end type

  type, BIND(C) :: RPN_MPI_Comm
    integer :: wrapped_value
  end type RPN_MPI_Comm

  type, BIND(C) :: RPN_MPI_Fcom    ! the Fortran trio, wrapped communicator, size, and rank
    type(RPN_MPI_Comm) :: comm
    integer(C_INT)     :: size
    integer(C_INT)     :: rank
  end type RPN_MPI_Fcom

  type, BIND(C) :: RPN_MPI_csr     ! the Fortran trio, communicator, size, and rank
    integer(C_INT)     :: comm
    integer(C_INT)     :: size
    integer(C_INT)     :: rank
  end type RPN_MPI_csr

  type, BIND(C) :: RPN_MPI_Datatype
    integer :: wrapped_value
  end type RPN_MPI_Datatype

  type, BIND(C) :: RPN_MPI_Errhandler
    integer :: wrapped_value
  end type RPN_MPI_Errhandler

  type, BIND(C) :: RPN_MPI_File
    integer :: wrapped_value
  end type RPN_MPI_File

  type, BIND(C) :: RPN_MPI_Group
    integer :: wrapped_value
  end type RPN_MPI_Group

  type, BIND(C) :: RPN_MPI_Info
    integer :: wrapped_value
  end type RPN_MPI_Info

  type, BIND(C) :: RPN_MPI_Message
    integer :: wrapped_value
  end type RPN_MPI_Message

  type, BIND(C) :: RPN_MPI_Op
    integer :: wrapped_value
  end type RPN_MPI_Op

  type, BIND(C) :: RPN_MPI_Request
    integer :: wrapped_value
  end type RPN_MPI_Request

  type, BIND(C) :: RPN_MPI_Win
    integer :: wrapped_value
  end type RPN_MPI_Win

!===================================================================
!  wrapped communicators, datatypes, operators
!  used to check interface prototypes
!  can be passed to RPN_MPI prototyped subroutines
!
!  CRITICAL:
!  the contents and component order of this type using "wrapped" components MUST BE IDENTICAL
!  to the contents of the "raw" type RPN_MPI_mpi_definitions_raw
!  the RPN_MPI_get_mpi_definitions and RPN_MPI_get_mpi_definitions_raw subroutines
!  RELY on this for correct behaviour
!
  type :: RPN_MPI_mpi_definitions
    integer :: version = mpi_symbols_version
    !
    !     some NULL "handles" not belonging to specific categories
    !
    type(RPN_MPI_Group)   :: MPI_GROUP_NULL
    type(RPN_MPI_Request) :: MPI_REQUEST_NULL
    integer               :: MPI_ERRHANDLER_NULL
    type(RPN_MPI_Info)    :: MPI_INFO_NULL
    type(RPN_MPI_Win)     :: MPI_WIN_NULL
    !
    !     varia
    !
    integer :: MPI_STATUS_SIZE
    integer :: MPI_ANY_SOURCE, MPI_ANY_TAG
    integer :: MPI_SUCCESS, MPI_ERROR
    integer :: MPI_ERRORS_ARE_FATAL, MPI_ERRORS_RETURN
    !
    !     communicators
    !
    type(RPN_MPI_Comm) :: MPI_COMM_NULL
    type(RPN_MPI_Comm) :: MPI_COMM_WORLD, MPI_COMM_SELF
    type(RPN_MPI_Comm) :: MPI_GROUP_EMPTY, MPI_COMM_TYPE_SHARED
    !
    !     data types
    !
    type(RPN_MPI_Datatype) :: MPI_DATATYPE_NULL
    type(RPN_MPI_Datatype) :: MPI_BYTE, MPI_PACKED, MPI_UB, MPI_LB
    type(RPN_MPI_Datatype) :: MPI_CHARACTER, MPI_LOGICAL
    type(RPN_MPI_Datatype) :: MPI_INTEGER, MPI_INTEGER1, MPI_INTEGER2, MPI_INTEGER4
    type(RPN_MPI_Datatype) :: MPI_INTEGER8, MPI_INTEGER16
    type(RPN_MPI_Datatype) :: MPI_REAL, MPI_REAL4, MPI_REAL8, MPI_REAL16
!     type(RPN_MPI_Datatype) :: MPI_REAL2
    type(RPN_MPI_Datatype) :: MPI_DOUBLE_PRECISION
    type(RPN_MPI_Datatype) :: MPI_COMPLEX, MPI_COMPLEX8, MPI_COMPLEX16, MPI_COMPLEX32
    type(RPN_MPI_Datatype) :: MPI_DOUBLE_COMPLEX
    type(RPN_MPI_Datatype) :: MPI_2REAL, MPI_2DOUBLE_PRECISION, MPI_2INTEGER
!     type(RPN_MPI_Datatype) :: MPI_2COMPLEX, MPI_2DOUBLE_COMPLEX
!     type(RPN_MPI_Datatype) :: MPI_LOGICAL1, MPI_LOGICAL2, MPI_LOGICAL4, MPI_LOGICAL8
    !
    !     operators
    !
    type(RPN_MPI_Op) :: MPI_OP_NULL
    type(RPN_MPI_Op) :: MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, MPI_LAND
    type(RPN_MPI_Op) :: MPI_BAND, MPI_LOR, MPI_BOR, MPI_LXOR, MPI_BXOR
    type(RPN_MPI_Op) :: MPI_MAXLOC, MPI_MINLOC, MPI_REPLACE
    !
    !     threads
    !
    integer :: MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED, MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE
  end type
!===================================================================
!
!  access to most useful MPI parameters without dependency upon MPI version via mpif.h or equivalent
!
!  USAGE :
!    include 'RPN_MPI_mpi_definitions.inc'
!    type(RPN_MPI_mpi_definitions) :: P
!    integer :: ierr
!    call RPN_MPI_get_mpi_definitions(P, ierr)
!
!    MPI parameters are now available
!    P%MPI_COMM_SELF  may be used for MPI_COMM_SELF
!    P%MPI_SUCCESS    may be used for MPI_SUCCESS
!    P%MPI_ERROR      may be used for MPI_ERROR
!
!  no need to recompile the user code when changing version of MPI, 
!  the RPN_MPI library acts as an insulation layer
!
!  user defined type containing frequently used MPI symbols
!
  type :: RPN_MPI_mpi_definitions_raw
    integer :: version = mpi_symbols_version
    !
    !     some NULL "handles" not belonging to specific categories
    !
    integer :: MPI_GROUP_NULL
    integer :: MPI_REQUEST_NULL
    integer :: MPI_ERRHANDLER_NULL
    integer :: MPI_INFO_NULL
    integer :: MPI_WIN_NULL
    !
    !     varia
    !
    integer :: MPI_STATUS_SIZE
    integer :: MPI_ANY_SOURCE, MPI_ANY_TAG
    integer :: MPI_SUCCESS, MPI_ERROR
    integer :: MPI_ERRORS_ARE_FATAL, MPI_ERRORS_RETURN
    !
    !     communicators
    !
    integer :: MPI_COMM_NULL
    integer :: MPI_COMM_WORLD, MPI_COMM_SELF
    integer :: MPI_GROUP_EMPTY, MPI_COMM_TYPE_SHARED
    !
    !     data types
    !
    integer :: MPI_DATATYPE_NULL
    integer :: MPI_BYTE, MPI_PACKED, MPI_UB, MPI_LB
    integer :: MPI_CHARACTER, MPI_LOGICAL
    integer :: MPI_INTEGER, MPI_INTEGER1, MPI_INTEGER2, MPI_INTEGER4
    integer :: MPI_INTEGER8, MPI_INTEGER16
    integer :: MPI_REAL, MPI_REAL4, MPI_REAL8, MPI_REAL16
!     integer :: MPI_REAL2
    integer :: MPI_DOUBLE_PRECISION
    integer :: MPI_COMPLEX, MPI_COMPLEX8, MPI_COMPLEX16, MPI_COMPLEX32
    integer :: MPI_DOUBLE_COMPLEX
    integer :: MPI_2REAL, MPI_2DOUBLE_PRECISION, MPI_2INTEGER
!     integer :: MPI_2COMPLEX, MPI_2DOUBLE_COMPLEX
!     integer :: MPI_LOGICAL1, MPI_LOGICAL2, MPI_LOGICAL4, MPI_LOGICAL8
    !
    !     operators
    !
    integer :: MPI_OP_NULL
    integer :: MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, MPI_LAND
    integer :: MPI_BAND, MPI_LOR, MPI_BOR, MPI_LXOR, MPI_BXOR
    integer :: MPI_MAXLOC, MPI_MINLOC, MPI_REPLACE
    !
    !     threads
    !
    integer :: MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED, MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE
  end type

