#define MPI_SUCCESS 0
      subroutine RPN_COMM_split_by_socket(origcomm, nodecomm, sockcomm, peercomm, noderank, sockrank, peerrank, isiz, err)   
      implicit none
      integer, intent(IN)  :: origcomm  ! MPI communicator to split on a socket basis        
      integer, intent(OUT) :: nodecomm  ! new communicator to be used py PEs on same node    
      integer, intent(OUT) :: sockcomm  ! new communicator to be used py PEs on same socket  
      integer, intent(OUT) :: peercomm  ! new communicator for socket peers                  
      integer, intent(OUT) :: noderank  ! rank in node communicator                          
      integer, intent(OUT) :: sockrank  ! rank in socket communicator                        
      integer, intent(OUT) :: peerrank  ! rank in socket peers                               
      integer, intent(OUT) :: isiz      ! size of socket communicator                        
      integer, intent(OUT) :: err       ! error code                                         
      nodecomm = origcomm
      sockcomm = origcomm
      peercomm = origcomm
      noderank = 0
      sockrank = 0
      peerrank = 0
      isiz = 1
      err = MPI_SUCCESS
      return
      end
      subroutine RPN_COMM_split_by_node(origcomm, nodecomm, peercomm, noderank, peerrank, isiz, err)   
      implicit none
      integer, intent(IN)  :: origcomm  ! MPI communicator to split on a host basis        
      integer, intent(OUT) :: nodecomm  ! new communicator to be used py PEs on same host  
      integer, intent(OUT) :: peercomm  ! communicator for node peers                      
      integer, intent(OUT) :: noderank  ! rank in new communicator                         
      integer, intent(OUT) :: peerrank  ! rank in node peers                               
      integer, intent(OUT) :: isiz      ! size of new communicator                         
      integer, intent(OUT) :: err       ! error code                                       
      nodecomm = origcomm
      peercomm = origcomm
      noderank = 0
      peerrank = 0
      isiz = 1
      err = MPI_SUCCESS
      return
      end
      subroutine RPN_COMM_win_allocate_shared(comm, msize, win, baseptr, err)
      use ISO_C_BINDING
      implicit none
      integer, intent(IN)  :: comm               !   communicator
      integer, intent(IN)  :: msize              !   size in integers of shared memory area
      integer, intent(OUT) :: win                !   window communicator
      type(C_PTR), intent(OUT) :: baseptr        !   base address of shared memory area
      integer, intent(OUT)  :: err               !   status RPN_COMM_OK or RPN_COMM_ERROR
      integer, dimension(:), pointer :: temp
      allocate(temp(msize))
      baseptr = C_LOC(temp(1))
      win = comm
      err = MPI_SUCCESS
      return
      end

