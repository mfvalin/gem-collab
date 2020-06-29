#if defined(STAND_ALONE)
program test_104
call rpn_mpi_test_104
end
#endif

subroutine rpn_mpi_test_104
!
! simple, standalone test for RPN_MPI_ez_halo_parms
! does not use the full RPN_MPI setup
!
  use ISO_C_BINDING
  implicit none
  include 'RPN_MPI.inc'
#define LoC(what) rpn_mpi_loc(loc(what))
!   include 'RPN_MPI_mpif.inc'

  integer :: ier, npe, rank
  type(RPN_MPI_mpi_definitions_raw) :: dr
  integer :: i, j, k, n, larg, stat, lni, lnj, lnk, gni, gnj, gnk, npex, npey, ranki, rankj
  integer :: i0, j0, k0, i0y, lniy, lnimaxy, ez
  character(len=128) :: argv(6), mode
  integer :: lnimax, lnjmax, lnkmax, errors
  integer(kind=8), dimension(:,:,:), allocatable   :: z, z2        ! original and restored original
  integer(kind=8), dimension(:), allocatable       :: zgi, zgj     ! full row, full column
  integer(kind=8), dimension(:,:,:,:), allocatable :: zt, zy, zty
  integer :: row_comm, col_comm
  integer :: myrow, mycol
  real(kind=8) :: time1, time2, time3, time4, time5
  real(kind=8), external :: MPI_Wtime

  call MPI_Init(ier)

  call RPN_MPI_get_mpi_definitions_raw(dr, ier)       ! get "raw" definitions
  call MPI_Comm_size(dr%MPI_COMM_WORLD, npe, ier)     ! get world size
  call MPI_Comm_rank(dr%MPI_COMM_WORLD, rank, ier)    ! get world rank

  do i=1,6
    call get_command_argument(i,argv(i),larg,stat)    ! get 5 program arguments
    if(stat .ne. 0) goto 777
  enddo
  write(0,*) 'I am PE',rank+1,' of',npe
  read(argv(1),*,err=777) gni                         ! global grid (gni, gnj, gnk)
  read(argv(2),*,err=777) gnj
  read(argv(3),*,err=777) gnk
  read(argv(4),*,err=777) npex                        ! PE topology (npex, npey)
  read(argv(5),*,err=777) npey
  read(argv(6),*,err=777) ez                          ! ez test flag (.ne. 0, use ez calls + setup
  if(npex*npey .ne. npe) then
    write(0,*) 'got',npe,' PEs, expected',npex*npey
    goto 777
  endif

  ranki = mod(rank,npex)
  mycol = ranki
  call MPI_Comm_split(dr%MPI_COMM_WORLD, mycol, rank, col_comm, ier)  ! column communicator
  rankj = rank / npex
  myrow = rankj
  call MPI_Comm_split(dr%MPI_COMM_WORLD, myrow, rank, row_comm, ier)  ! row communicator

  lnimax = (gni + npex -1) / npex      ! gni distributed over x
  i0 = lnimax * ranki
  lni = min(lnimax, gni - (lnimax * ranki))
  lni = max(0,lni)

  lnimaxy = (gni + npey -1) / npey     ! gni distributed over y
  i0y = lnimaxy * rankj
  lniy = min(lnimaxy, gni - (lnimaxy * ranki))
  lniy = max(0,lniy)

  lnjmax = (gnj + npey -1) / npey      ! gnj distributed over y
  j0 = lnjmax * rankj
  lnj = min(lnjmax, gnj - (lnjmax * rankj))
  lnj = max(0,lnj)

  lnkmax = (gnk + npex -1) / npex      ! gnk distributed over x
  k0 = lnkmax * ranki
  lnk = min(lnkmax, gnk - (lnkmax * ranki))
  lnk = max(0,lnk)

  write(0,*) 'gni, gnj, gnk, lni, lnj, lnk =',gni, gnj, gnk, lni, lnj, lnk
  write(0,*) 'ranki, rankj, lnimax, lnimaxy =',ranki, rankj, lnimax, lnimaxy

  allocate( z(lnimax,lnjmax,gnk))                 ! original array
  allocate(z2(lnimax,lnjmax,gnk))                 ! restored original array
  allocate(zt(lnimax,lnjmax,max(1,lnk),npex))     ! after x transpose
  allocate( zgi(lnimax*npex))                     ! a full row, used to reshape zt into zy, and zy into zt
  allocate( zy(lnimaxy,lnjmax,max(1,lnk),npey))   ! before y transpose, after inverse y transpose
  allocate(zty(lnimaxy,lnjmax,max(1,lnk),npey))   ! after y transpose

  do k = 1, gnk                                   ! fill with 'tag' values iiijjjkkk
  do j = 1, lnj
  do i = 1, lni
    z(i,j,k) = (i0+i)*1000000 + (j0+j)*1000 + k
  enddo
  do i = lni+1, lnimax
    z(i,j,k) = 0
  enddo
  enddo
  enddo
!
! conditional ez transpose setup call
!
  if(ez .ne. 0) call RPN_MPI_transpose_setup(gnk, lnk, row_comm, col_comm, ier)
!   if(ez .eq. 0) call RPN_MPI_transpose_setup(gnk, lnk, row_comm, col_comm, ier)
!
! fwd x, fwd y, inverse y, inverse x transposes (lni values are doubled because z is integer*8)
!
  time1 = MPI_Wtime()
  if(ez .eq. 0) call RPN_MPI_transpose_xz(LoC(z), LoC(zt), .true., lnimax*2, lnjmax, gnk, lnk, row_comm, ier)
  if(ez .eq. 0) call RPN_MPI_transpose_xz(LoC(z), LoC(zt), .true., lnimax*2, lnjmax, gnk, lnk, row_comm, ier)
  if(ez .eq. 0) call RPN_MPI_transpose_xz(LoC(z), LoC(zt), .true., lnimax*2, lnjmax, gnk, lnk, row_comm, ier)
  if(ez .eq. 0) call RPN_MPI_transpose_xz(LoC(z), LoC(zt), .true., lnimax*2, lnjmax, gnk, lnk, row_comm, ier)
! use ez call if setup has been performed by RPN_MPI_transpose_setup
  if(ez .ne. 0) call RPN_MPI_ez_transpose_xz(LoC(z), LoC(zt), .true., lnimax*2, lnjmax, lnk, ier)
  if(ez .ne. 0) call RPN_MPI_ez_transpose_xz(LoC(z), LoC(zt), .true., lnimax*2, lnjmax, lnk, ier)
  if(ez .ne. 0) call RPN_MPI_ez_transpose_xz(LoC(z), LoC(zt), .true., lnimax*2, lnjmax, lnk, ier)
  if(ez .ne. 0) call RPN_MPI_ez_transpose_xz(LoC(z), LoC(zt), .true., lnimax*2, lnjmax, lnk, ier)
  time1 = MPI_Wtime() - time1

! reshape (lnix,lnjy,lnkx,npex) -> (lniy,lnjy,lnkx,npey)
  do k = 1,lnk
  do j = 1,lnjmax
    do n = 0,npex-1    ! (1:lnix,i,j,1:npex) -> (gni)
    do i = 1,lnimax
      zgi (n*lnimax+i) = zt(i,j,k,n+1)   ! gather full row
    enddo
    enddo
    do n = 0,npey-1    ! (gni) -> (1:lniy,i,j,1:npey) 
    do i = 1,lnimaxy
      zy(i,j,k,n+1) = zgi (n*lnimaxy+i)  ! scatter full row
    enddo
    enddo
  enddo
  enddo

  time2 = MPI_Wtime()
  if(ez .eq. 0) call RPN_MPI_transpose_xy(LoC(zy), LoC(zty), .true.,  lnimaxy*2, lnjmax, lnk, col_comm, ier)
  if(ez .eq. 0) call RPN_MPI_transpose_xy(LoC(zy), LoC(zty), .true.,  lnimaxy*2, lnjmax, lnk, col_comm, ier)
  if(ez .eq. 0) call RPN_MPI_transpose_xy(LoC(zy), LoC(zty), .true.,  lnimaxy*2, lnjmax, lnk, col_comm, ier)
  if(ez .eq. 0) call RPN_MPI_transpose_xy(LoC(zy), LoC(zty), .true.,  lnimaxy*2, lnjmax, lnk, col_comm, ier)
! use ez call if setup has been performed by RPN_MPI_transpose_setup
  if(ez .ne. 0) call RPN_MPI_ez_transpose_xy(LoC(zy), LoC(zty), .true.,  lnimaxy*2, lnjmax, lnk, ier)
  if(ez .ne. 0) call RPN_MPI_ez_transpose_xy(LoC(zy), LoC(zty), .true.,  lnimaxy*2, lnjmax, lnk, ier)
  if(ez .ne. 0) call RPN_MPI_ez_transpose_xy(LoC(zy), LoC(zty), .true.,  lnimaxy*2, lnjmax, lnk, ier)
  if(ez .ne. 0) call RPN_MPI_ez_transpose_xy(LoC(zy), LoC(zty), .true.,  lnimaxy*2, lnjmax, lnk, ier)
  time2 = MPI_Wtime() - time2
  zy = 0
!   call RPN_MPI_transpose_xy(LoC(zy), LoC(zty), .false., lnimaxy*2, lnjmax, lnk, col_comm, ier)
! use ez call because previous RPN_MPI_transpose_xy has performed initialization
  time3 = MPI_Wtime()
  call RPN_MPI_ez_transpose_xy(LoC(zy), LoC(zty), .false., lnimaxy*2, lnjmax, lnk, ier)
  call RPN_MPI_ez_transpose_xy(LoC(zy), LoC(zty), .false., lnimaxy*2, lnjmax, lnk, ier)
  call RPN_MPI_ez_transpose_xy(LoC(zy), LoC(zty), .false., lnimaxy*2, lnjmax, lnk, ier)
  call RPN_MPI_ez_transpose_xy(LoC(zy), LoC(zty), .false., lnimaxy*2, lnjmax, lnk, ier)
  time3 = MPI_Wtime() - time3

! reshape (lniy,lnjy,lnkx,npey) -> (lnix,lnjy,lnkx,npex)
  do k = 1,lnk
  do j = 1,lnjmax
    do n = 0,npey-1    ! (1:lniy,j,k,1:npey) -> (gni)
    do i = 1,lnimaxy
      zgi (n*lnimaxy+i) = zy(i,j,k,n+1)   ! gather full row
    enddo
    enddo
    do n = 0,npex-1    ! (gni) -> (1:lnix,j,k,1:npex) 
    do i = 1,lnimax
      zt(i,j,k,n+1) = zgi (n*lnimax+i)    ! scatter full row
    enddo
    enddo
  enddo
  enddo

  z2 = 0
!   call RPN_MPI_transpose_xz(LoC(z2), LoC(zt), .false., lnimax*2, lnjmax, gnk, lnk, row_comm, ier)
! use ez call because previous RPN_MPI_transpose_xz has performed initialization
  time4 = MPI_Wtime()
  call RPN_MPI_ez_transpose_xz(LoC(z2), LoC(zt), .false., lnimax*2, lnjmax, lnk, ier)
  call RPN_MPI_ez_transpose_xz(LoC(z2), LoC(zt), .false., lnimax*2, lnjmax, lnk, ier)
  call RPN_MPI_ez_transpose_xz(LoC(z2), LoC(zt), .false., lnimax*2, lnjmax, lnk, ier)
  call RPN_MPI_ez_transpose_xz(LoC(z2), LoC(zt), .false., lnimax*2, lnjmax, lnk, ier)
  time4 = MPI_Wtime() - time4
  if(gni <= 16) then
    write(0,*) 'after inverse x transpose'
    do j = lnj,1,-1
      write(0,2)(z2(1:lnimax,j,k),k=1,gnk)
    enddo
  endif

  errors = 0
  do k = 1, gnk
  do j = 1, lnj
  do i = 1, lni
    if(z(i,j,k) .ne. z2(i,j,k)) errors = errors + 1
  enddo
  enddo
  enddo
  write(0,*) 'errors =',errors
!   if(rank == 0) write(0,3) 'times =', time1, time2, time3, time4
  if(rank == 0) call RPN_MPI_print_transpose_times

1 call MPI_Finalize(ier)
  return

777 continue
  write(0,*) 'ERROR in arguments'
  goto 1
2 format(30I10.9)
3 format(A,10F10.6)
end subroutine rpn_mpi_test_104
