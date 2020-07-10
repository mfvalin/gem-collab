#if defined(STAND_ALONE)
program test_105
call rpn_mpi_test_105
end
#endif

subroutine rpn_mpi_test_105
!
! simple, standalone test for RPN_MPI_grid_topo
! does not use the full RPN_MPI setup
!
  use ISO_C_BINDING
  implicit none
#include <RPN_MPI.hf>
#define LoC(what) rpn_mpi_loc(loc(what))

  integer :: ier, npe, rank, larg
  type(RPN_MPI_mpi_definitions_raw) :: dr
  integer :: i, j, npex, npey, blkx, blky, stat, x_first, ez
  character(len=128) :: argv(6)
  type(RPN_MPI_Ftopo) :: topo
  integer, dimension(:,:), allocatable :: pe_matrix, pe_matrix_ref
  integer, dimension(:), allocatable :: pe_x, pe_y

  call MPI_Init(ier)

  call RPN_MPI_get_mpi_definitions_raw(dr, ier)       ! get "raw" definitions
  call MPI_Comm_size(dr%MPI_COMM_WORLD, npe, ier)     ! get world size
  call MPI_Comm_rank(dr%MPI_COMM_WORLD, rank, ier)    ! get world rank

  do i=1,6
    call get_command_argument(i,argv(i),larg,stat)    ! get 5 program arguments
    if(stat .ne. 0) goto 777
  enddo
  read(argv(1),*,err=777) npex                        ! PE topology (npex, npey)
  read(argv(2),*,err=777) npey
  read(argv(3),*,err=777) blkx                        ! block topology (blkx, blky)
  read(argv(4),*,err=777) blky
  read(argv(5),*,err=777) x_first                     ! spread first along x if 1
  read(argv(6),*,err=777) ez                          ! ez test flag (.ne. 0, use ez calls + setup
  allocate( pe_matrix(0:npex-1,0:npey-1), pe_x(0:npe-1), pe_y(0:npe-1) )
  allocate( pe_matrix_ref(0:npex-1,0:npey-1))
  pe_matrix = -1
  pe_x      = -1
  pe_y      = -1

  if(rank == 0) then                                   ! RPN_MPI_grid_topo test using MPI_COMM_NULL
    if(x_first .eq. 1)then
      write(0,*) 'INFO: distributing PEs along X axis first'
    else
      write(0,*) 'INFO: distributing PEs along Y axis first'
    endif
    if(ez .ne. 0) then
        write(0,*) 'INFO: testing EZ_grid_topo with prior setup'
    else
        write(0,*) 'INFO: testing grid_topo without prior setup'
    endif
    topo%grd%comm = RPN_MPI_Comm(dr%MPI_COMM_NULL)
    topo%grd%size = npex * npey
    do i = 0, topo%grd%size - 1                       ! loop over simulated ranks
      topo%grd%rank = i
      if(ez .ne. 0) then
        call RPN_MPI_set_grid_topo(npex, npey, blkx, blky, x_first == 1)
        call RPN_MPI_ez_grid_topo(topo, ier)
      else
        call RPN_MPI_grid_topo(topo, npex, npey, blkx, blky, x_first == 1, ier)
      endif
      if(topo%row%rank > npex - 1) goto 777
      if(topo%row%rank < 0       ) goto 777
      if(topo%col%rank > npey - 1) goto 777
      if(topo%col%rank < 0       ) goto 777
      pe_matrix(topo%row%rank, topo%col%rank) = i
    enddo
    write(0,3) ' (Y)'
    do j = npey-1, 0, -1
      write(0,2) j, ' |', pe_matrix(:,j)
    enddo
    write(0,4) ' ','+',('----------',i=1,npex)
    write(0,3) '  (X)',(i, i=0,npex-1)
    pe_matrix_ref = pe_matrix
  endif
  if(npe == 1) goto 1
  pe_matrix = -1
  write(0,*) 'I am PE',rank+1,' of',npe

  topo%grd%comm = RPN_MPI_Comm(dr%MPI_COMM_WORLD)     ! grid communicator (MPI_COMM_WORLD)
  topo%grd%comm%wrapped_value = dr%MPI_COMM_WORLD     ! alternative syntax (not recommended)
  topo%grd%rank = rank
  topo%grd%size = npe

  if(npex*npey .ne. npe) then
    write(0,*) 'got',npe,' PEs, expected',npex*npey
    goto 777
  endif

  if(ez .ne. 0) then
    call RPN_MPI_set_grid_topo(npex, npey, blkx, blky, x_first == 1)
    call RPN_MPI_ez_grid_topo(topo, ier)
  else
    call RPN_MPI_grid_topo(topo, npex, npey, blkx, blky, x_first == 1, ier)
  endif

  call MPI_Gather(topo%row%rank, 1, dr%MPI_INTEGER, pe_x, 1, dr%MPI_INTEGER, 0, topo%grd%comm, ier)  ! gather x positions on Pe 0
  call MPI_Gather(topo%col%rank, 1, dr%MPI_INTEGER, pe_y, 1, dr%MPI_INTEGER, 0, topo%grd%comm, ier)  ! gather y positions on Pe 0

1 continue
  if(rank == 0) then
    do i = 0, topo%grd%size - 1
      if(pe_x(i) < 0 .or. pe_y(i) < 0 .or. pe_x(i) > npex-1 .or. pe_y(i) > npey -1) goto 777
      pe_matrix(pe_x(i), pe_y(i)) = i
    enddo
    write(0,3) ' (Y)'
    do j = npey-1, 0, -1
      write(0,2) j, ' |', pe_matrix(:,j)
    enddo
    write(0,4) ' ','+',('----------',i=1,npex)
    write(0,3) '  (X)',(i, i=0,npex-1)
    if( any(pe_matrix .ne. pe_matrix_ref) ) then
      write(0,*) 'ERROR: inconsistent PE tables'
    else
      write(0,*) 'PASSED: PE tables are coherent'
    endif
  endif

  call MPI_Finalize(ier)
  return

777 continue
  write(0,*) 'ERROR in arguments'
  goto 1
2 format(I4,A2,30I10)
3 format(A5,1X,29I10)
4 format(A5,A1,29A10)
end subroutine rpn_mpi_test_105
