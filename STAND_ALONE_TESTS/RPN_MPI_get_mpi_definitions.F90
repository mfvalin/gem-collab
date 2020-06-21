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
module RPN_MPI_mpi_layout
  use ISO_C_BINDING
  use rpn_mpi_mpif
  implicit none
  include 'RPN_MPI_mpi_symbols.inc'
  include 'RPN_MPI_mpi_layout.inc'

  ! ml is statically initialized, mw will be initialized by RPN_MPI_init_mpi_layout
  type(mpi_layout_internal), save, target :: ml = &    ! RPN_MPI communicators, ranks, sizes
    mpi_layout_internal( &
      layout_version, &
      -1, -1, &                               ! host, numa node
      [-1, -1, -1], &                         ! colors
      grid_hierarchy( &                       ! communicators
	application(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL), &
	application(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL), &
	mpigrid(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, &
	        MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, &
	        MPI_COMM_NULL, MPI_COMM_NULL), &
	mpigrid(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, &
	        MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, &
	        MPI_COMM_NULL, MPI_COMM_NULL), &
	subgrid(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL) &
      ), &
      grid_hierarchy( &                       ! ranks
	application(-1, -1, -1), &
	application(-1, -1, -1), &
	mpigrid(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1), &
	mpigrid(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1), &
	subgrid(-1, -1, -1) &
      ), &
      grid_hierarchy( &                       ! sizes
	application(-1, -1, -1), &
	application(-1, -1, -1), &
	mpigrid(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1), &
	mpigrid(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1), &
	subgrid(-1, -1, -1) &
      ) &
    )
  type(mpi_layout), save, pointer         :: mw => NULL()    ! communicators, ranks, sizes, wrapped

  ! MPI constants, dr is statically initialized, dw is initialized by RPN_MPI_init_mpi_layout
  type(RPN_MPI_mpi_definitions_raw), save, target :: dr    = RPN_MPI_mpi_definitions_raw( &
    mpi_symbols_version, &
    MPI_GROUP_NULL, MPI_REQUEST_NULL,MPI_ERRHANDLER_NULL, MPI_INFO_NULL,  MPI_WIN_NULL, &
    MPI_STATUS_SIZE, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_SUCCESS, MPI_ERROR, &
    MPI_ERRORS_ARE_FATAL, MPI_ERRORS_RETURN, &
    MPI_COMM_NULL, MPI_COMM_WORLD, MPI_COMM_SELF, MPI_GROUP_EMPTY, MPI_COMM_TYPE_SHARED, &
    MPI_DATATYPE_NULL, &
    MPI_BYTE, MPI_PACKED, MPI_UB, MPI_LB, &
    MPI_CHARACTER, MPI_LOGICAL, MPI_INTEGER, MPI_INTEGER1, MPI_INTEGER2, MPI_INTEGER4, &
    MPI_INTEGER8, MPI_INTEGER16, MPI_REAL, MPI_REAL4, MPI_REAL8, MPI_REAL16, &
    MPI_DOUBLE_PRECISION, MPI_COMPLEX, MPI_COMPLEX8, MPI_COMPLEX16, MPI_COMPLEX32, &
    MPI_DOUBLE_COMPLEX, MPI_2REAL, MPI_2DOUBLE_PRECISION, MPI_2INTEGER, &
    MPI_OP_NULL, &
    MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, MPI_LAND, MPI_BAND, &
    MPI_LOR, MPI_BOR, MPI_LXOR, MPI_BXOR, MPI_MAXLOC, MPI_MINLOC, MPI_REPLACE, &
    MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED, MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE &
    )
  type(RPN_MPI_mpi_definitions), save, pointer :: dw => NULL()   ! MPI constants, "wrapped"
contains
  subroutine RPN_MPI_init_mpi_layout     ! MUST BE CALLED ASAP by RPN_MPI_init
    implicit none
    include 'RPN_MPI_system_interfaces.inc'
    integer :: cpu
    type(C_PTR) :: p

    p = C_LOC(dr)
    call C_F_POINTER(p, dw)

!     ml%version = layout_version
    ml%host = get_host_id()              ! get linux host id
    cpu = sched_get_my_cpu()             ! get logical cpu number
    ml%numa = numa_node(cpu)             ! get numa space number for this cpu
!     ml%colors = [-1, -1, -1]
! 
!     ml%comm%wrld = application(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL)
!     ml%rank%wrld = application(-1, -1, -1)
!     ml%size%wrld = application(-1, -1, -1)
! 
!     ml%comm%appl = ml%comm%wrld
!     ml%rank%appl = ml%rank%wrld
!     ml%size%appl = ml%size%wrld
! 
!     ml%comm%sgrd = mpigrid(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, &
! 			   MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, &
! 			   MPI_COMM_NULL, MPI_COMM_NULL)
!     ml%rank%sgrd = mpigrid(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
!     ml%size%sgrd = mpigrid(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
! 
!     ml%comm%grid = ml%comm%sgrd
!     ml%rank%grid = ml%rank%sgrd
!     ml%size%grid = ml%size%sgrd
! 
!     ml%comm%blck = subgrid(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL)
!     ml%rank%blck = subgrid(-1, -1, -1)
!     ml%size%blck = subgrid(-1, -1, -1)

    p = C_LOC(ml)
    call C_F_POINTER(p, mw)
    return
  end subroutine RPN_MPI_init_mpi_layout
end module RPN_MPI_mpi_layout

 subroutine RPN_MPI_get_mpi_definitions_raw(what, ierr)          !InTf! ! get a copy of raw MPI definitions
  use ISO_C_BINDING
  use RPN_MPI_mpi_layout
  implicit none
!!  import :: RPN_MPI_mpi_definitions_raw                        !InTf! 
  type(RPN_MPI_mpi_definitions_raw), intent(INOUT) :: what      !InTf! 
  integer, intent(OUT) :: ierr                                  !InTf! 

  ierr = MPI_ERROR
  if(what%version .ne. mpi_symbols_version) then
    write(0,*)'ERROR: (RPN_MPI_get_mpi_definitions_raw) version mismatch, expected :',mpi_symbols_version,' got :',what%version
    what%version = -1
    return
  endif
  ierr = MPI_SUCCESS
  what = dr
  return
 end subroutine RPN_MPI_get_mpi_definitions_raw          !InTf!

 subroutine RPN_MPI_get_mpi_definitions(what, ierr)          !InTf! ! get a copy of wrapped MPI definitions
  use ISO_C_BINDING
  use RPN_MPI_mpi_layout
  implicit none
!!  import :: RPN_MPI_mpi_definitions                        !InTf! 
  type(RPN_MPI_mpi_definitions), intent(INOUT) :: what      !InTf! 
  integer, intent(OUT) :: ierr                              !InTf! 

  ierr = MPI_ERROR
  if(what%version .ne. mpi_symbols_version) then
    write(0,*)'ERROR: (RPN_MPI_get_mpi_definitions) version mismatch, expected :',mpi_symbols_version,' got :',what%version
    what%version = -1
    return
  endif
  ierr = MPI_SUCCESS
  what = transfer(dr,dw)

  return
 end subroutine RPN_MPI_get_mpi_definitions                  !InTf! 

! initialize RPN_MPI internal mpi layout (communicators, ranks, sizes)
! OBSOLETE NOW, replaced by RPN_MPI_init_mpi_layout
 subroutine RPN_MPI_reset_mpi_layout()                       !InTf! 
  use RPN_MPI_mpi_layout
  implicit none
  include 'RPN_MPI_system_interfaces.inc'
  integer :: cpu
  call RPN_MPI_init_mpi_layout

!   ml%host = get_host_id()              ! get linux host id
!   cpu = sched_get_my_cpu()             ! get logical cpu number
!   ml%numa = numa_node(cpu)             ! get numa space number for this cpu
!   ml%colors = [-1, -1, -1]
! 
!   ml%comm%wrld = application(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL)
!   ml%rank%wrld = application(-1, -1, -1)
!   ml%size%wrld = application(-1, -1, -1)
! 
!   ml%comm%appl = ml%comm%wrld
!   ml%rank%appl = ml%rank%wrld
!   ml%size%appl = ml%size%wrld
! 
!   ml%comm%sgrd = mpigrid(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, &
! 			 MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL, &
! 			 MPI_COMM_NULL, MPI_COMM_NULL)
!   ml%rank%sgrd = mpigrid(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
!   ml%size%sgrd = mpigrid(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
! 
!   ml%comm%grid = ml%comm%sgrd
!   ml%rank%grid = ml%rank%sgrd
!   ml%size%grid = ml%size%sgrd
! 
!   ml%comm%blck = subgrid(MPI_COMM_NULL, MPI_COMM_NULL, MPI_COMM_NULL)
!   ml%rank%blck = subgrid(-1, -1, -1)
!   ml%size%blck = subgrid(-1, -1, -1)

  return
 end subroutine RPN_MPI_reset_mpi_layout                     !InTf! 

! same as RPN_MPI_get_mpi_layout but advertized with type mpi_layout_internal rather than mpi_layout
! get a copy on RPN_MPI internal mpi layout (communicators, ranks, sizes)
 subroutine RPN_MPI_get_mpi_layout_raw(what, ierr)            !InTf! 
  use RPN_MPI_mpi_layout
  implicit none
!!  import :: mpi_layout_internal, C_INT                      !InTf! 
  type(mpi_layout_internal), intent(INOUT) :: what           !InTf! 
  integer(C_INT), intent(OUT) :: ierr                        !InTf! 
  call RPN_MPI_get_mpi_layout(what, ierr)
  return
 end subroutine RPN_MPI_get_mpi_layout_raw                    !InTf! 

! get a copy on RPN_MPI internal mpi layout (communicators, ranks, sizes)
 subroutine RPN_MPI_get_mpi_layout(what, ierr)               !InTf! 
  use RPN_MPI_mpi_layout
  implicit none
! white lie in published interface, "what" is treated as the "internal" type rather than the "wrapped" type
!!  import :: mpi_layout, C_INT                              !InTf! 
!!  type(mpi_layout), intent(INOUT) :: what                  !InTf! 
  type(mpi_layout_internal), intent(INOUT) :: what
  integer(C_INT), intent(OUT) :: ierr                       !InTf! 
  integer :: i

  ierr = MPI_ERROR
  if(what%version .ne. layout_version) then
    write(0,*)'ERROR: (RPN_MPI_get_mpi_layout) version mismatch, expected :',layout_version,' got :',what%version
    what%version = -1
    return
  endif
  ierr = MPI_SUCCESS

  what = ml
  what%comm%sgrd%row       = MPI_COMM_NULL   ! row is not defined for supergrids
  what%rank%sgrd%row       = -1
  what%size%sgrd%row       = -1
  what%comm%sgrd%column    = MPI_COMM_NULL   ! and neither is column
  what%rank%sgrd%column    = -1
  what%size%sgrd%column    = -1
  return
 end subroutine RPN_MPI_get_mpi_layout                       !InTf! 
