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
!     set PE block topology
!     assemble PEs in blocks of blkx by blky tiles
!
! x_first == .true. :
!     PEs within a block : along X first, then along Y
!     blocks on nodes : X direction first, then Y direction
!     (a block is blkx PEs by blky PEs)
!
! x_first == .false. :
!     PEs within a block : along Y first, then along X
!     blocks on nodes : Y direction first, then X direction
!     (a block is blkx PEs by blky PEs)
!
! examples: 
!          8 by 8 model grid, blkx=4, blky=4,  (block fill)
!          x_first would have no impact in this case
!          16 processes per node, 64 processes (00-63)
!
!  topo%col%rank  (node 2)               (node 3)
!         +----+----+----+----+  +----+----+----+----+
!    7    | 44 | 45 | 46 | 47 |  | 60 | 61 | 62 | 63 |
!         +----+----+----+----+  +----+----+----+----+
!    6    | 40 | 41 | 42 | 43 |  | 56 | 57 | 58 | 59 |
!         +----+----+----+----+  +----+----+----+----+
!    5    | 36 | 37 | 38 | 39 |  | 52 | 53 | 54 | 55 |
!         +----+----+----+----+  +----+----+----+----+
!    4    | 32 | 33 | 34 | 35 |  | 48 | 49 | 50 | 51 |
!         +----+----+----+----+  +----+----+----+----+
!
!  topo%col%rank  (node 0)              (node 1)
!         +----+----+----+----+  +----+----+----+----+
!    3    | 12 | 13 | 14 | 15 |  | 28 | 29 | 30 | 31 |
!         +----+----+----+----+  +----+----+----+----+
!    2    | 08 | 09 | 10 | 11 |  | 24 | 25 | 26 | 27 |
!         +----+----+----+----+  +----+----+----+----+
!    1    | 04 | 05 | 06 | 07 |  | 20 | 21 | 22 | 23 |
!         +----+----+----+----+  +----+----+----+----+
!    0    | 00 | 01 | 02 | 03 |  | 16 | 17 | 18 | 19 |
!         +----+----+----+----+  +----+----+----+----+
!            0    1    2    3    4    5    6    7    topo%row%rank
!---------------------------------------------------------------------
!          8 by 8 model grid, blkx=2, blky=4,  (block fill)
!          x_first would have no impact in this case
!          16 processes per node, 64 processes (00-63)
!
!  topo%col%rank  (node 2)               (node 3)
!         +----+----+----+----+  +----+----+----+----+
!    7    | 38 | 39 | 46 | 47 |  | 54 | 55 | 62 | 63 |
!         +----+----+----+----+  +----+----+----+----+
!    6    | 36 | 37 | 44 | 45 |  | 52 | 53 | 60 | 61 |
!         +----+----+----+----+  +----+----+----+----+
!    5    | 34 | 35 | 42 | 43 |  | 50 | 51 | 58 | 59 |
!         +----+----+----+----+  +----+----+----+----+
!    4    | 32 | 33 | 40 | 41 |  | 48 | 49 | 56 | 57 |
!         +----+----+----+----+  +----+----+----+----+
!
!                 (node 0)              (node 1)
!         +----+----+----+----+  +----+----+----+----+
!    3    | 06 | 07 | 14 | 15 |  | 22 | 23 | 30 | 31 |
!         +----+----+----+----+  +----+----+----+----+
!    2    | 04 | 05 | 12 | 13 |  | 20 | 21 | 28 | 29 |
!         +----+----+----+----+  +----+----+----+----+
!    1    | 02 | 03 | 10 | 11 |  | 18 | 19 | 26 | 27 |
!         +----+----+----+----+  +----+----+----+----+
!    0    | 00 | 01 | 08 | 09 |  | 16 | 17 | 24 | 25 |
!         +----+----+----+----+  +----+----+----+----+
!            0    1    2    3    4    5    6    7    topo%row%rank
!---------------------------------------------------------------------
!          8 by 8 model grid, blkx=2, blky=2,  (block fill)
!          x_first is .true.
!          16 processes per node, 64 processes (00-63)
!
!  topo%col%rank
!         +----+----+----+----+----+----+----+----+
!    7    | 50 | 51 | 54 | 55 | 58 | 59 | 62 | 63 |
!         +----+----+----+----+----+----+----+----+ (node 3)
!    6    | 48 | 49 | 52 | 53 | 56 | 57 | 60 | 61 |
!         +----+----+----+----+----+----+----+----+
!
!         +----+----+----+----+----+----+----+----+
!    5    | 34 | 35 | 38 | 39 | 41 | 43 | 46 | 47 |
!         +----+----+----+----+----+----+----+----+ (node 2)
!    4    | 32 | 33 | 36 | 37 | 40 | 42 | 44 | 45 |
!         +----+----+----+----+----+----+----+----+
!
!         +----+----+----+----+----+----+----+----+
!    3    | 18 | 19 | 22 | 23 | 26 | 27 | 30 | 31 |
!         +----+----+----+----+----+----+----+----+ (node 1)
!    2    | 16 | 17 | 20 | 21 | 24 | 25 | 28 | 29 |
!         +----+----+----+----+----+----+----+----+
!
!         +----+----+----+----+----+----+----+----+
!    1    | 02 | 03 | 06 | 07 | 10 | 11 | 14 | 15 |
!         +----+----+----+----+----+----+----+----+ (node 0)
!    0    | 00 | 01 | 04 | 05 | 08 | 09 | 12 | 13 |
!         +----+----+----+----++----+----+----+----+
!            0    1    2    3    4    5    6    7    topo%row%rank
!---------------------------------------------------------------------
!          8 by 8 model grid, blkx=2, blky=2,  (block fill)
!          x_first is .false.
!          16 processes per node, 64 processes (00-63)
!
!  topo%col%rank
!             (0)          (1)          (2)          (3)     (node)
!         +----+----+  +----+----+  +----+----+  +----+----+
!    7    | 13 | 15 |  | 29 | 31 |  | 45 | 47 |  | 61 | 63 |
!         +----+----+  +----+----+  +----+----+  +----+----+
!    6    | 12 | 14 |  | 28 | 30 |  | 44 | 46 |  | 60 | 62 |
!         +----+----+  +----+----+  +----+----+  +----+----+
!    5    | 09 | 11 |  | 25 | 27 |  | 41 | 43 |  | 57 | 59 |
!         +----+----+  +----+----+  +----+----+  +----+----+
!    4    | 08 | 10 |  | 24 | 26 |  | 40 | 42 |  | 56 | 58 |
!         +----+----+  +----+----+  +----+----+  +----+----+
!    3    | 05 | 07 |  | 21 | 23 |  | 37 | 39 |  | 53 | 55 |
!         +----+----+  +----+----+  +----+----+  +----+----+
!    2    | 04 | 06 |  | 20 | 22 |  | 36 | 38 |  | 52 | 54 |
!         +----+----+  +----+----+  +----+----+  +----+----+
!    1    | 01 | 03 |  | 17 | 19 |  | 33 | 35 |  | 49 | 51 |
!         +----+----+  +----+----+  +----+----+  +----+----+
!    0    | 00 | 02 |  | 16 | 18 |  | 32 | 34 |  | 48 | 50 |
!         +----+----+  +----+----+  +----+----+  +----+----+
!            0    1       2    3       4    5       6    7    topo%row%rank
!---------------------------------------------------------------------
!          4 by 4 model grid, blkx=4, blky=1, (horizontal fill)
!          x_first would have no impact in this case
!          x_first = .true., blkx=1, blky=1 would yield the same result
!          4 processes per node, 16 processes (00-15)
!
!  topo%col%rank
!         +----+----+----+----+
!    3    | 12 | 13 | 14 | 15 | (node 3)
!         +----+----+----+----+
!         +----+----+----+----+
!    2    | 08 | 09 | 10 | 11 | (node 2)
!         +----+----+----+----+
!         +----+----+----+----+
!    1    | 04 | 05 | 06 | 07 | (node 1)
!         +----+----+----+----+
!         +----+----+----+----+
!    0    | 00 | 01 | 02 | 03 | (node 0)
!         +----+----+----+----+ 
!            0    1    2    3     topo%row%rank
!   
!          4 by 4 model grid, blkx=1, blky=4, (vertical fill)
!          x_first = .false., blkx=1, blky=1 would yield the same result
!          4 processes per node, 16 processes (00-15)
!---------------------------------------------------------------------
!  topo%col%rank
!           (0)     (1)     (2)     (3)   (node) 
!         +----+  +----+  +----+  +----+
!    3    | 03 |  | 07 |  | 11 |  | 15 |
!         +----+  +----+  +----+  +----+
!    2    | 02 |  | 06 |  | 10 |  | 14 |
!         +----+  +----+  +----+  +----+
!    1    | 01 |  | 05 |  | 09 |  | 13 |
!         +----+  +----+  +----+  +----+
!    0    | 00 |  | 04 |  | 08 |  | 12 |
!         +----+  +----+  +----+  +----+ 
!            0       1       2       3    topo%row%rank
!
! internal data for RPN_MPI_ez_grid_topo
module RPN_MPI_mod_grid_topo
  integer, save :: pex = 0
  integer, save :: pey = 0
  integer, save :: bkx = 0
  integer, save :: bky = 0
  logical, save :: along_x = .true.
end module RPN_MPI_mod_grid_topo
! setup internal data for RPN_MPI_ez_grid_topo
 subroutine RPN_MPI_set_grid_topo(npex, npey, blkx, blky, x_first)   !InTf!
  use RPN_MPI_mod_grid_topo
  implicit none
  integer, intent(IN) :: npex, npey                    !InTf!
  integer, intent(IN) :: blkx, blky                    !InTf!
  logical, intent(IN) :: x_first                       !InTf!
  pex = npex
  pey = npey
  bkx = blkx
  bky = blky
  along_x = x_first
 end subroutine RPN_MPI_set_grid_topo                   !InTf!
! split a grid into npey rows and npex columns,
! using blocks of blkx by blky PEs
! x_first true means spread in the X direction first
! x_first false means spread in the Y direction first
! if blkx is larger than npex, it is set internally to npex
! if blky is larger than npey, it is set internally to npey
! e.g. blky = 999999999 means go along the full column (npey)
!      blkx = 999999999 means go along the full row (npex)
! if all goes well, ierr = MPI_OK, 
! the row and column communicators, sizes, and ranks have been set properly
!
! if topo%grd%comm is MPI_COMM_NULL, the row and column communicators
! are set to MPI_COMM_NULL, but the topology is computed according to input
! number of PEs is taken from topo%grd%size, and rank from topo%grd%rank
! this can be useful to check the grid distribution across PEs (and for internal tests)
!
! CAVEAT: except if topo%grd%comm is MPI_COMM_NULL this routine is a COLLECTIVE operator
! in the topo%grd%comm communicator
 subroutine RPN_MPI_grid_topo(topo, npex, npey, blkx, blky, x_first, ierr)   !InTf!
  use ISO_C_BINDING
  use rpn_mpi_mpif
  implicit none
  include 'RPN_MPI_mpi_symbols.inc'
  include 'RPN_MPI_mpi_layout.inc'
!! import :: RPN_MPI_Ftopo                             !InTf!
  type(RPN_MPI_Ftopo), intent(INOUT) :: topo           !InTf!
  integer, intent(IN) :: npex, npey                    !InTf!
  integer, intent(IN) :: blkx, blky                    !InTf!
  logical, intent(IN) :: x_first                       !InTf!
  integer, intent(OUT) :: ierr                         !InTf!

  integer :: blockx, blocky, blocks, block_x, block_y, block_me
  integer :: block_rank, brank_x, brank_y, blocksx, blocksy, blocksize

  blockx = min(blkx, npex)                      ! block size along x
  blocky = min(blky, npey)                      ! block size along y

  if(topo%grd%size <= 0 .or. topo%grd%rank < 0) then        ! missing rank and/or size
    call MPI_Comm_size(topo%grd%comm, topo%grd%size, ierr)  ! grep grid size
    if(ierr .ne. MPI_SUCCESS) return
    call MPI_Comm_rank(topo%grd%comm, topo%grd%rank, ierr)  ! grep rank in grid
    if(ierr .ne. MPI_SUCCESS) return
  endif

  ierr = MPI_ERROR
  if(topo%version .ne. mpi_symbols_version) return   ! RPN_MPI version mismatch
  if(npex*npey .ne. topo%grd%size) return       ! PE number mismatch
  if(mod(npex,blockx) .ne. 0)      return       ! npex not a multiple of blockx
  if(mod(npey,blocky) .ne. 0)      return       ! npey not a multiple of blocky

  blocksize  = blockx*blocky
  blocks     = topo%grd%size / blocksize        ! number of grid blocks
  blocksx    = npex / blockx                    ! number of grid blocks along x
  blocksy    = npey / blocky                    ! number of grid blocks along x
  block_me   = topo%grd%rank / blocksize        ! my grid block ordinal
  block_rank = mod(topo%grd%rank, blocksize)    ! my rank in my block

  if(x_first) then                              ! along X first for PE allocation
    block_x = mod(block_me, blocksx)            ! ordinal of my block along x
    block_y = block_me / blocksx                ! ordinal of my block along y
    brank_x = mod(block_rank, blockx)           ! my ordinal along x in my block
    brank_y = block_rank / blockx               ! my ordinal along y in my block

  else                                          ! along Y first for PE allocation
    block_y = mod(block_me, blocksy)            ! ordinal of my block along y
    block_x = block_me / blocksy                ! ordinal of my block along x
    brank_y = mod(block_rank, blocky)           ! my ordinal along y in my block
    brank_x = block_rank / blocky               ! my ordinal along x in my block
  endif

  topo%row%rank = block_x * blockx + brank_x    ! my rank in row (along X)
  topo%row%size = npex                          ! size of row
  topo%col%rank = block_y * blocky + brank_y    ! my rank in column (along y)
  topo%col%size = npey                          ! size of column
! split grid communicator into row and column communicators
! members of a row have same rank in column (topo%col%rank)
! members of a column have same rank in row (topo%row%rank)
  if(topo%grd%comm%wrapped_value .ne. MPI_COMM_NULL) then
!     	 MPI_COMM_SPLIT(COMM         , COLOR        , KEY          , NEWCOMM      , IERR)
    call MPI_Comm_split(topo%grd%comm, topo%col%rank, topo%grd%rank, topo%row%comm, ierr)
    call MPI_Comm_split(topo%grd%comm, topo%row%rank, topo%grd%rank, topo%col%comm, ierr)
  else
    topo%row%comm = RPN_MPI_Comm(MPI_COMM_NULL)
    topo%col%comm = RPN_MPI_Comm(MPI_COMM_NULL)
  endif

 end subroutine RPN_MPI_grid_topo                      !InTf!
 subroutine RPN_MPI_ez_grid_topo(topo, ierr)           !InTf!
  use ISO_C_BINDING
  use rpn_mpi_mpif
  use RPN_MPI_mod_grid_topo
  implicit none
#define IN_RPN_MPI_grid_topo
#include <RPN_MPI.hf>
!! import :: RPN_MPI_Ftopo                            !InTf!
  type(RPN_MPI_Ftopo), intent(INOUT) :: topo          !InTf!
  integer, intent(OUT) :: ierr                        !InTf!

  ierr = MPI_ERROR
  call RPN_MPI_grid_topo(topo, pex, pey, bkx, bky, along_x, ierr)
 end subroutine RPN_MPI_ez_grid_topo                   !InTf!
