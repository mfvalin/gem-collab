! * Copyright (C) 2020  Recherche en Prevision Numerique
! *
! * This software is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This software is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.

#if ! defined(SELF_TEST)
module RPN_COMM_halo_cache
  integer, save :: gridcom, rowcom, colcom
  integer, save :: rankx, ranky, sizex, sizey
  integer(kind=8), dimension(6) :: ts = [0,0,0,0,0,0]
  integer, save :: nhalo = 0
  logical, save :: redblack = .false.  ! if true, use fully synchronous (send, recv, no sendrecv) method
  logical, save :: async    = .false.  ! if true, use fully asynchronous (isend, irecv)  method
end module RPN_COMM_halo_cache

function RPN_COMM_get_halo_timings(t,n) result(nt)
  use RPN_COMM_halo_cache
  implicit none
  integer, intent(IN) :: n
  integer(kind=8), dimension(n), intent(OUT) :: t
  integer :: nt
  t(1:n) = -1
  nt = -1
  if(n < 6) return
  nt = nhalo
  t(1:6) = ts
end function RPN_COMM_get_halo_timings

subroutine RPN_COMM_reset_halo_timings
  use RPN_COMM_halo_cache
  implicit none
  nhalo = 0
  ts = 0
end subroutine RPN_COMM_reset_halo_timings

subroutine RPN_COMM_print_halo_timings
  use RPN_COMM_halo_cache
  implicit none
  write(6,1)'nhalo, times =',nhalo,ts/nhalo
  call flush(6)
1 format(a,10I10)
end subroutine RPN_COMM_print_halo_timings

! set communicators for halo exchange
subroutine RPN_COMM_simple_halo_parms(grid, row, col, mode)
  use RPN_COMM_halo_cache
  implicit none
  integer, intent(IN) :: grid, row, col
  character(len=*), intent(IN) :: mode   
  integer :: ier
  gridcom = grid  ! grid communicator
  rowcom  = row   ! row communicator
  colcom  = col   ! column communicator
  call MPI_comm_size(rowcom, sizex, ier)  ! size of row
  call MPI_comm_rank(rowcom, rankx, ier)  ! rank in row
  call MPI_comm_size(colcom, sizey, ier)  ! size of column
  call MPI_comm_rank(colcom, ranky, ier)  ! rank in column
  redblack = trim(mode) .eq. 'REDBLACK'
  async    = trim(mode) .eq. 'ASYNC'
  if(rankx+ranky == 0) write(6,*) 'MODE = '//trim(mode)
  return
end subroutine RPN_COMM_simple_halo_parms

subroutine RPN_COMM_simple_halo_8(g,minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy)
  implicit none
  integer, intent(IN)    :: minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy
  integer, intent(INOUT), dimension(2*minx-1:2*maxx,miny:maxy,nk) :: g
  call RPN_COMM_simple_halo(g,2*minx-1,2*maxx,miny,maxy,2*lni,lnj,nk,2*halox,haloy)
end subroutine RPN_COMM_simple_halo_8

! RED/BLACK method, only synchronous send and recv
!   West -> East move
!     EVEN PEs
!       if not East PE send to odd PE at East 
!       if not West PE recv from odd PE at West 
!     ODD PEs
!                      recv from even PE at West
!       if not East PE send to even PE at East
!   West <- East move
!     EVEN PEs
!       if not East PE recv from odd PE at East
!       if not West PE send to odd PE at West
!     ODD PEs
!                      send to even PE at West
!       if not East PE recv from even PE at East
subroutine RPN_COMM_simple_halo(g,minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy)
  use ISO_C_BINDING
  use RPN_COMM_halo_cache
  implicit none
  include 'mpif.h'
  integer, intent(IN)    :: minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy
  integer, intent(INOUT), dimension(minx:maxx,miny:maxy,nk) :: g

  integer :: i, j, k, nw, ier
  integer, dimension(MPI_STATUS_SIZE) :: status
  integer, dimension(halox,lnj,nk) :: halo_from_west, halo_to_west
  integer, dimension(halox,lnj,nk) :: halo_from_east, halo_to_east
  integer, dimension(halox+lni+halox,haloy,nk) :: halo_from_north, halo_to_north
  integer, dimension(halox+lni+halox,haloy,nk) :: halo_from_south, halo_to_south
  integer(kind=8),dimension(10) :: t
  integer(kind=8), external :: cpu_real_time_ticks
  integer :: nreq
  integer, dimension(4) :: requests
  integer, dimension(MPI_STATUS_SIZE,4) :: statuses

  t = 0
  nhalo = nhalo + 1
  if(sizex .gt. 1) then   ! is there an exchange along x ?
    call MPI_Barrier(rowcom,ier)
    nw = halox * lnj * nk       ! message length
    if(async) then              ! post receives as early in the game as possible
      nreq = 0                  ! number of asynchronous requests
      if(rankx > 0) then         ! not west PE
        nreq = nreq + 1
        call MPI_Irecv(halo_from_west, nw, MPI_INTEGER, rankx-1, rankx-1, rowcom, requests(nreq), ier)    ! receive from west neighbor
      endif
      if(rankx < sizex-1) then  ! not east PE
        nreq = nreq + 1
        call MPI_Irecv(halo_from_east, nw, MPI_INTEGER, rankx+1, rankx+1, rowcom, requests(nreq), ier)    ! receive from east neighbor
      endif
    endif
    t(1) = cpu_real_time_ticks()
    do k = 1, nk          ! peel west and east side of array simultaneously
    do j = 1, lnj
      halo_to_west(:,j,k) = g(1:halox        ,j,k)    ! extract west side inner halo
      halo_to_east(:,j,k) = g(lni+1-halox:lni,j,k)    ! extract east side inner halo
    enddo
    enddo
    t(2) = cpu_real_time_ticks()
    if(redblack)then   ! reb/black method, West -> East then West <- East
      if(iand(rankx,1) .eq. 0) then   ! even ranks 
        if(rankx < sizex-1) call MPI_Send(halo_to_east  , nw, MPI_INTEGER, rankx+1, rankx, &
                            rowcom,ier) ! send to east if not east PE      (West -> East)
        if(rankx > 0)       call MPI_Recv(halo_from_west, nw, MPI_INTEGER, rankx-1, rankx-1, &
                            rowcom, STATUS,ier) ! recv from west if not west
        if(rankx < sizex-1) call MPI_Recv(halo_from_east, nw, MPI_INTEGER, rankx+1, rankx+1, &
                            rowcom, STATUS,ier) ! receive from east if not east PE (West <- East)
        if(rankx > 0)       call MPI_Send(halo_to_west  , nw, MPI_INTEGER, rankx-1, rankx, &
                            rowcom,ier) ! send to west if not west PE
      else                            ! odd ranks
                            call MPI_Recv(halo_from_west, nw, MPI_INTEGER, rankx-1, rankx-1, &
                            rowcom, STATUS,ier)    ! receive from west             (West -> East)
        if(rankx < sizex-1) call MPI_Send(halo_to_east  , nw, MPI_INTEGER, rankx+1, rankx, &
                            rowcom,ier)    ! send to east if not east PE
                            call MPI_Send(halo_to_west  , nw, MPI_INTEGER, rankx-1, rankx, &
                            rowcom,ier)    ! send to west                  (West <- East)
        if(rankx < sizex-1) call MPI_Recv(halo_from_east, nw, MPI_INTEGER, rankx+1, rankx+1, &
                            rowcom, STATUS,ier)    ! receive from east if not east PE
      endif
    elseif(async) then    ! fully asynchronous methos
      if(rankx > 0) then         ! not west PE
        nreq = nreq + 1
        call MPI_Isend(halo_to_west, nw, MPI_INTEGER, rankx-1, rankx, rowcom, requests(nreq), ier)    ! send to west neighbor
      endif
      if(rankx < sizex-1) then  ! not east PE
        nreq = nreq + 1
        call MPI_Isend(halo_to_east, nw, MPI_INTEGER, rankx+1, rankx, rowcom, requests(nreq), ier)    ! send to east neighbor
      endif
      call MPI_Waitall(nreq, requests, statuses, ier)
    else     ! default method with sendrecv
      if(rankx .eq. 0)then             ! west PE, send to east, get from east
	call MPI_SENDRECV(halo_to_east  , nw, MPI_INTEGER, rankx+1, rankx, &
			  halo_from_east, nw, MPI_INTEGER, rankx+1, rankx+1, &
			  rowcom, STATUS, ier)
      elseif(rankx .lt. sizex-1) then  ! middle PE, (get from west, send to east) (get from east, send to west)
	call MPI_SENDRECV(halo_to_east  , nw, MPI_INTEGER, rankx+1, rankx, &
			  halo_from_west, nw, MPI_INTEGER, rankx-1, rankx-1, &
			  rowcom, STATUS, ier)
	call MPI_SENDRECV(halo_to_west  , nw, MPI_INTEGER, rankx-1, rankx, &
			  halo_from_east, nw, MPI_INTEGER, rankx+1, rankx+1, &
			  rowcom, STATUS, ier)
      else                             ! east PE, send to west, get from west
	call MPI_SENDRECV(halo_to_west  , nw, MPI_INTEGER, rankx-1, rankx, &
			  halo_from_west, nw, MPI_INTEGER, rankx-1, rankx-1, &
			  rowcom, STATUS, ier)
      endif
    endif
    t(3) = cpu_real_time_ticks()
    do k = 1, nk          ! insert west and east side of array simultaneously
    do j = 1, lnj
      if(rankx .gt. 0)       g(1-halox:0,j,k)       = halo_from_west(:,j,k)     ! insert into west side outer halo
      if(rankx .lt. sizex-1) g(lni+1:lni+halox,j,k) = halo_from_east(:,j,k)     ! insert into east side outer halo
    enddo
    enddo
    t(4) = cpu_real_time_ticks()
  endif
  if(sizey .gt. 1) then   ! is there an exchange along y ?
    call MPI_Barrier(colcom, ier)
    t(5) = cpu_real_time_ticks()
    nw = (lni + 2*halox) * haloy * nk
    do k = 1, nk          ! peel north and south side of array
    do j = 1, haloy
      halo_to_north(:,j,k) = g(1-halox:lni+halox,lnj-haloy+j,k)    ! extract north side inner halo
      halo_to_south(:,j,k) = g(1-halox:lni+halox,j          ,k)    ! extract south side inner halo
    enddo
    enddo
    t(6) = cpu_real_time_ticks()
    if(ranky .eq. 0) then                ! south PE, send to north, receive from north
      call MPI_SENDRECV(halo_to_north  , nw, MPI_INTEGER, ranky+1, ranky, &
                        halo_from_north, nw, MPI_INTEGER, ranky+1, ranky+1, &
                        colcom, STATUS, ier)
    elseif(ranky .lt. sizey-1) then      ! middle PE,(send to north, get from south) (send to south, get from north)
      call MPI_SENDRECV(halo_to_north  , nw, MPI_INTEGER, ranky+1, ranky, &
                        halo_from_south, nw, MPI_INTEGER, ranky-1, ranky-1, &
                        colcom, STATUS, ier)
      call MPI_SENDRECV(halo_to_south  , nw, MPI_INTEGER, ranky-1, ranky, &
                        halo_from_north, nw, MPI_INTEGER, ranky+1, ranky+1, &
                        colcom, STATUS, ier)
    else                                 ! north PE, send to south, receive from south
      call MPI_SENDRECV(halo_to_south  , nw, MPI_INTEGER, ranky-1, ranky, &
                        halo_from_south, nw, MPI_INTEGER, ranky-1, ranky-1, &
                        colcom, STATUS, ier)
    endif
    t(7) = cpu_real_time_ticks()
    do k = 1, nk          ! peel north and south side of array
    do j = 1, haloy
      if(ranky .lt. sizey-1) g(1-halox:lni+halox,lnj+j  ,k) = halo_from_north(:,j,k)  ! not north row insert into north outer halo
      if(ranky .gt. 0)       g(1-halox:lni+halox,j-haloy,k) = halo_from_south(:,j,k)  ! not south row insert into south outer halo
    enddo
    enddo
    t(8) = cpu_real_time_ticks()
  endif
  ts(1:3) = ts(1:3) + t(2:4)-t(1:3)
  ts(4:6) = ts(4:6) + t(6:8)-t(5:7)
end subroutine RPN_COMM_simple_halo
#endif
#if defined(SELF_TEST)
program test_simple_halo
  use ISO_C_BINDING
  implicit none
  include 'mpif.h'
!   integer, parameter :: NI = 5
!   integer, parameter :: NJ = 3
!   integer, parameter :: NK = 1
!   integer, parameter :: halox = 1
!   integer, parameter :: haloy = 1
  integer :: NI, NJ, NK, halox, haloy
  integer, dimension(:,:,:), allocatable :: z
  integer :: rankx, sizex, ranky, sizey, ier, petot, ranktot, errors
  integer :: i, j, k, offx, offy, l, m, larg1, larg2, larg3, stat1, stat2, stat3, rowcomm, colcomm
  integer :: ilo, ihi, jlo, jhi
  character(len=128) :: argv1, argv2, argv3, mode
  logical :: printit, redblack, yfirst, async
  real(kind=8) :: t1, t2

  call MPI_Init(ier)
  printit = .false.
  call get_command_argument(1,argv1,larg1,stat1)
  call get_command_argument(2,argv2,larg2,stat2)
  call get_command_argument(3,argv3,larg3,stat3)
  if(stat1 .ne. 0 .or. stat2 .ne. 0) goto 777
  read(argv1,*,err=777)sizex,sizey
  read(argv2,*,err=777)NI, NJ, NK, halox, haloy
  mode = 'DEFAULT'
  yfirst = .false.
  if(stat3 .eq. 0) then
    printit  = argv3(1:1) .eq. 't'  ! print arrays
    redblack = argv3(2:2) .eq. 'r'  ! use red/black method
    async    = argv3(2:2) .eq. 'a'  ! use async method
    yfirst   = argv3(3:3) .eq. 'y'  ! y first, then x
  else
    argv3 = 't'
  endif
  if(redblack) mode = 'REDBLACK'
  if(async)    mode = 'ASYNC'

  allocate (z(1-halox:NI+halox,1-haloy:NJ+haloy,NK))

  call MPI_comm_size(MPI_COMM_WORLD, petot, ier)
  call MPI_comm_rank(MPI_COMM_WORLD, ranktot, ier)
  if(sizex*sizey .ne. petot) goto 777
  if(ranktot == 0) write(6,*)'redblack =',redblack
  if(yfirst) then
    rankx = ranktot / sizey
    ranky = mod(ranktot,sizey)
  else
    ranky = ranktot / sizex
    rankx = mod(ranktot,sizex)
  endif
  do l=0,petot-1
    if(ranktot .eq. l .and. argv3(1:1) .eq. 't') write(6,2)'ranktot, petot, rankx, ranky =',ranktot+1, petot, rankx, ranky
    call MPI_Barrier(MPI_COMM_WORLD,ier)
  enddo
  call MPI_Comm_split(MPI_COMM_WORLD, ranky, ranktot, rowcomm,ier)
  call MPI_Comm_split(MPI_COMM_WORLD, rankx, ranktot, colcomm,ier)

  call RPN_COMM_simple_halo_parms(MPI_COMM_WORLD, rowcomm, colcomm, mode)

  offy = ranky * NJ
  offx = rankx * NI
  z = 0
  do k=1,NK
  do j=1,NJ
  do i=1,NI
    z(i,j,k) = 1000000*mod(i+offx,256) + 1000*mod(j+offy,256) + k
  enddo
  enddo
  enddo
  call MPI_Barrier(MPI_COMM_WORLD,ier)
  call RPN_COMM_simple_halo(z,1-halox,NI+halox,1-haloy,NJ+haloy,NI,NJ,NK,halox,haloy)
  call RPN_COMM_reset_halo_timings
  call MPI_Barrier(MPI_COMM_WORLD,ier)
  t1 = MPI_Wtime()
  do i = 1, 20
    call RPN_COMM_simple_halo(z,1-halox,NI+halox,1-haloy,NJ+haloy,NI,NJ,NK,halox,haloy)
  enddo
  call MPI_Barrier(MPI_COMM_WORLD,ier)
  t2 = MPI_Wtime()
  call MPI_Barrier(MPI_COMM_WORLD,ier)

  do i = 0, petot-1, max(petot/16,1)
    if(ranktot .eq. i) call RPN_COMM_print_halo_timings
  enddo
  call MPI_Barrier(MPI_COMM_WORLD,ier)
  if(printit) then
    do m = sizey-1, 0, -1
    do l = sizex-1, 0, -1
      if(ranktot .eq. l+m*sizex)then
	write(6,*)"PE(",rankx,',',ranky,')'
	do j=NJ+haloy,1-haloy,-1
	  print 1,z(:,j,1)
	enddo
      endif
      call MPI_Barrier(MPI_COMM_WORLD,ier)
    enddo
    enddo
  endif
  errors = 0
  ilo = 1 - halox
  jlo = 1 - haloy
  ihi = NI + halox
  jhi = NJ + haloy
  if(ranky .eq. 0) jlo = 1         ! south PE
  if(ranky .eq. sizey-1) jhi = NJ  ! north PE
  if(rankx .eq. 0) ilo = 1         ! west PE
  if(rankx .eq. sizex-1) ihi = NI  ! east PE
  do k = 1, nk
  do j = jlo, jhi
  do i = ilo, ihi
    if(z(i,j,k) .ne. 1000000*mod(i+offx,256) + 1000*mod(j+offy,256) + k) errors = errors + 1
  enddo
  enddo
  enddo
  call flush(6)
  call MPI_Barrier(MPI_COMM_WORLD,ier)
  if(ranktot .eq. 0) write(6,*)'time per exchange =',nint((t2-t1) * 100000.0),' microseconds'
  if(errors .ne. 0) write(6,*)'rank, ERRORS =',ranktot, errors
777 continue
  call MPI_Finalize(ier)
1 format(20I10.9)
2 format(A,20i6)
end program
#endif
