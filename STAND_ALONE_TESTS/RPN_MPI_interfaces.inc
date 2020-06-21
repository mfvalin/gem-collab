interface

      SUBROUTINE RPN_COMM_barrier(com,ierr)                    
      implicit none                                            
      integer, intent(OUT) :: ierr                             
      character(len=*), intent(IN) ::  com                     
      end SUBROUTINE RPN_COMM_barrier                                

      integer function RPN_COMM_barrier_data(level,values,nvalues)                                
      integer , intent(IN) :: level
      integer , intent(IN) :: nvalues                                                              
      real(kind=kind(1.d0)), dimension(nvalues), intent(OUT) :: values                                           
      end function RPN_COMM_barrier_data                                            

      integer function RPN_COMM_colors(comm)         
      implicit none                                  
      character(len=*) :: comm                       
      end function RPN_COMM_colors                    

      integer function RPN_COMM_comm(com)                    
      implicit none                                 
      character(len=*), intent(IN) :: com           
      end function RPN_COMM_comm                                  

      SUBROUTINE RPN_COMM_finalize(ierr) 
      implicit none                      
      integer, intent(OUT) ::  ierr      
      end SUBROUTINE RPN_COMM_finalize         


      SUBROUTINE RPN_COMM_init(Userinit,Pelocal,Petotal,Pex,Pey)     
      implicit none                                                  
      integer, intent(out)   :: Pelocal,Petotal                      
      integer, intent(inout) :: Pex,Pey                              
      external Userinit                                              
      end SUBROUTINE RPN_COMM_init                                   

      INTEGER FUNCTION RPN_COMM_init_multigrid   &                   
         (Userinit,Pelocal,Petotal,Pex,Pey,MultiGrids)             
      implicit none                                                  
      external :: Userinit                                           
      integer, intent(out)   :: Pelocal,Petotal                      
      integer, intent(inout) :: Pex,Pey                              
      integer, intent(in)    :: MultiGrids                           
      end FUNCTION RPN_COMM_init_multigrid                           

      INTEGER FUNCTION RPN_COMM_init_multi_level &                   
         (Userinit,Pelocal,Petotal,Pex,Pey,MultiGrids,Grids)    
      implicit none                                                  
      external :: Userinit                                           
      integer, intent(out)   :: Pelocal,Petotal                      
      integer, intent(inout) :: Pex,Pey                              
      integer, intent(in)    :: MultiGrids                           
      integer, intent(in)    :: Grids                                
      end FUNCTION RPN_COMM_init_multi_level                        
      integer function RPN_COMM_get_a_free_unit_x()                   
      end function RPN_COMM_get_a_free_unit_x                         
      function RPN_COMM_set_timeout_alarm_x(seconds) result(seconds_since)  
      integer, intent(IN) :: seconds  
      integer :: seconds_since  
      end function RPN_COMM_set_timeout_alarm_x                             

      integer function RPN_COMM_limit_2 &                           
         (my_id, npe, gmin, gmax, lmini,lmaxi,count, offset,relax)  
      implicit none                                                
      integer, intent(IN) ::  my_id, npe, gmin, gmax, relax         
      integer, intent(OUT) :: lmini,lmaxi                           
      integer, intent(OUT) :: count(npe),offset(npe)                
      end function RPN_COMM_limit_2                       

      integer function RPN_COMM_limit  &                   
         (my_id, npe, gmin, gmax, lmini, lmaxi,count, offset) 
      implicit none                                        
      integer, intent(IN) ::  my_id, npe, gmin, gmax       
      integer, intent(OUT) :: lmini,lmaxi                  
      integer, intent(OUT) :: count(npe),offset(npe)       
      end function RPN_COMM_limit                          

      SUBROUTINE RPN_COMM_UP2LOW(S1,S2)                
      implicit none                                    
      character (len=*), intent(IN) :: S1              
      character (len=*), intent(OUT) :: S2             
      END SUBROUTINE RPN_COMM_UP2LOW               

      SUBROUTINE RPN_COMM_rank( com, rank ,ierr )                   
      implicit none                                                 
      character(len=*),intent(in) :: com                            
      integer, intent(out) :: rank                                  
      integer, intent(out) :: ierr                                  
      end SUBROUTINE RPN_COMM_rank                                 

      SUBROUTINE RPN_COMM_size( com, size ,ierr ) 
      implicit none                                 
      integer, intent(OUT) :: ierr,size             
      character(len=*), intent(IN) ::  com          
      end SUBROUTINE RPN_COMM_size                  

      subroutine RPN_data_distribute(myrank, npe, gmin, gmax, lmin, lmax, count, offset, mode)    
      implicit none                                                
      integer, intent(IN) ::  myrank, npe, gmin, gmax               
      integer, intent(OUT) :: lmin, lmax                            
      integer, intent(OUT) :: count(npe), offset(npe)               
      integer, intent(IN), optional ::  mode                        
      end subroutine RPN_data_distribute                               
      subroutine RPN_MPI_halo(g,minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy,row,col) BIND(C,name='RPN_MPI_halo') 
      import :: RPN_MPI_Loc, RPN_MPI_Comm, C_INT                                          
      integer(C_INT), intent(IN)    :: minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy          
      type(RPN_MPI_Comm), intent(IN) :: row,col                                           
      type(RPN_MPI_Loc), intent(IN), value :: g                                           
      end subroutine RPN_MPI_halo                                               
      subroutine RPN_MPI_halo8(g,minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy,row,col) BIND(C,name='RPN_MPI_halo8')  
      import :: RPN_MPI_Loc, RPN_MPI_Comm, C_INT                                          
      integer(C_INT), intent(IN)    :: minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy          
      type(RPN_MPI_Comm), intent(IN) :: row,col                                           
      type(RPN_MPI_Loc), intent(IN), value :: g                                           
      end subroutine RPN_MPI_halo8                                                           
      subroutine RPN_MPI_ez_halo(g,minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy) bind(C,name='RPN_MPI_ez_halo')       
      import :: RPN_MPI_Loc                                                       
      integer, intent(IN)    :: minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy          
      type(RPN_MPI_Loc), intent(IN), value :: g                                   
      end subroutine RPN_MPI_ez_halo                                                  
      subroutine RPN_MPI_ez_halo8(g,minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy) bind(C,name='RPN_MPI_ez_halo8')     
      import :: RPN_MPI_Loc                                                       
      integer, intent(IN)    :: minx,maxx,miny,maxy,lni,lnj,nk,halox,haloy          
      type(RPN_MPI_Loc), intent(IN), value :: g                                   
      end subroutine RPN_MPI_ez_halo8                                                
      function RPN_MPI_get_halo_timings(t,n) result(nt) BIND(C,name='RPN_MPI_get_halo_timings')   
      integer, intent(IN) :: n                                                                  
      integer(kind=8), dimension(n), intent(OUT) :: t                                           
      integer :: nt                                                                             
      end function RPN_MPI_get_halo_timings                                                       
      subroutine RPN_MPI_reset_halo_timings() BIND(C,name='RPN_MPI_reset_halo_timings')   
      end subroutine RPN_MPI_reset_halo_timings                                           
      subroutine RPN_MPI_print_halo_timings() BIND(C,name='RPN_MPI_print_halo_timings')   
      end subroutine RPN_MPI_print_halo_timings                                           
      subroutine RPN_MPI_ez_halo_parms(row, col, mode) bind(C,name='RPN_MPI_ez_halo_parms')     
      import :: RPN_MPI_Comm                                  
      type(RPN_MPI_Comm), intent(IN) :: row, col               
      character(len=1), dimension(*), intent(IN) :: mode        
      end subroutine RPN_MPI_ez_halo_parms                        
      subroutine RPN_MPI_transpose_setup(gnk, lnkx, row_comm, col_comm, ierr) 
      integer, intent(IN) :: row_comm, col_comm                              
      integer, intent(IN) :: gnk, lnkx                                       
      integer, intent(OUT) :: ierr                                           
      end subroutine RPN_MPI_transpose_setup                                  
      subroutine RPN_MPI_ez_transpose_xz(z, zt, forward, lnix, lnjy, lnkx, ierr) 
      import :: RPN_MPI_Loc                                        
      type(RPN_MPI_Loc), intent(IN), value :: z, zt                
      logical, intent(IN) :: forward                                
      integer, intent(IN) :: lnix, lnjy, lnkx                       
      integer, intent(OUT) :: ierr                                  
      end subroutine RPN_MPI_ez_transpose_xz                         
      subroutine RPN_MPI_transpose_xz(z, zt, forward, lnix, lnjy, gnk, lnkx, row_comm, ierr) 
      import :: RPN_MPI_Loc                                        
      type(RPN_MPI_Loc), intent(IN), value :: z, zt                
      logical, intent(IN) :: forward                                
      integer, intent(IN) :: lnix, lnjy, gnk, lnkx                  
      integer, intent(IN) :: row_comm                               
      integer, intent(OUT) :: ierr                                  
      end subroutine RPN_MPI_transpose_xz 
      subroutine RPN_MPI_ez_transpose_xy(z, zt, forward, lniy, lnjy, lnkx, ierr) 
      import :: RPN_MPI_Loc                                        
      type(RPN_MPI_Loc), intent(IN), value :: z, zt                
      logical, intent(IN) :: forward                                
      integer, intent(IN) :: lniy, lnjy, lnkx                       
      integer, intent(OUT) :: ierr                                  
      end subroutine RPN_MPI_ez_transpose_xy                         
      subroutine RPN_MPI_transpose_xy(z, zt, forward, lniy, lnjy, lnkx, col_comm, ierr) 
      import :: RPN_MPI_Loc                                        
      type(RPN_MPI_Loc), intent(IN), value :: z, zt                
      logical, intent(IN) :: forward                                
      integer, intent(IN) :: lniy, lnjy, lnkx                       
      integer, intent(IN) :: col_comm                               
      integer, intent(OUT) :: ierr                                  
      end subroutine RPN_MPI_transpose_xy 

      SUBROUTINE RPN_MPI_finalize(ierr) 
      implicit none                      
      integer, intent(OUT) ::  ierr      
      end SUBROUTINE RPN_MPI_finalize         
      integer function RPN_MPI_get_a_free_unit()                   
      end function RPN_MPI_get_a_free_unit                         
      integer function RPN_COMM_get_a_free_unit()                   
      end function RPN_COMM_get_a_free_unit                         
      subroutine RPN_MPI_get_mpi_definitions_raw(what, ierr)
      import :: RPN_MPI_mpi_definitions_raw                         
      type(RPN_MPI_mpi_definitions_raw), intent(INOUT) :: what       
      integer, intent(OUT) :: ierr                                   
      end subroutine RPN_MPI_get_mpi_definitions_raw          
      subroutine RPN_MPI_get_mpi_definitions(what, ierr)
      import :: RPN_MPI_mpi_definitions                         
      type(RPN_MPI_mpi_definitions), intent(INOUT) :: what       
      integer, intent(OUT) :: ierr                               
      end subroutine RPN_MPI_get_mpi_definitions                   
      subroutine RPN_MPI_reset_mpi_layout()                        
      end subroutine RPN_MPI_reset_mpi_layout                      
      subroutine RPN_MPI_get_mpi_layout_raw(what, ierr)             
      import :: mpi_layout_internal, C_INT                       
      type(mpi_layout_internal), intent(INOUT) :: what            
      integer(C_INT), intent(OUT) :: ierr                         
      end subroutine RPN_MPI_get_mpi_layout_raw                     
      subroutine RPN_MPI_get_mpi_layout(what, ierr)                
      import :: mpi_layout, C_INT                               
      type(mpi_layout), intent(INOUT) :: what                   
      integer(C_INT), intent(OUT) :: ierr                        
      end subroutine RPN_MPI_get_mpi_layout                        

      INTEGER FUNCTION RPN_MPI_init &                                
         (Userinit,Pelocal,Petotal,Pex,Pey,MultiGrids,Grids,AppID,Io)   
      implicit none                                                  
      external :: Userinit                                           
      integer, intent(out)   :: Pelocal,Petotal                      
      integer, intent(inout) :: Pex,Pey                              
      integer, intent(in)    :: MultiGrids                           
      integer, intent(in)    :: Grids                                
      character(len=*)       :: AppID                                
      integer, intent(in)    :: Io                                   
      end FUNCTION RPN_MPI_init                        

      subroutine RPN_MPI_mydomain (call_back, mydomain)             
      external :: call_back                                          
      integer, intent(OUT) :: mydomain                               
      end subroutine RPN_MPI_mydomain                                
      subroutine RPN_COMM_mydomain (call_back, mydomain)             
      external :: call_back                                          
      integer, intent(OUT) :: mydomain                               
      end subroutine RPN_COMM_mydomain                               

      subroutine RPN_MPI_world_set(world_comm)                    
      integer, intent(IN) ::  world_comm                           
      end subroutine RPN_MPI_world_set                            
      subroutine RPN_COMM_world_set(world_comm)                    
      integer, intent(IN) ::  world_comm                           
      end subroutine RPN_COMM_world_set                            
      subroutine RPN_MPI_get_core_and_numa(core, numa)            
      integer, intent(OUT) ::  core, numa                          
      end subroutine RPN_MPI_get_core_and_numa                    
      function RPN_MPI_initialized() result(status)             
      logical ::  status                                        
      end function RPN_MPI_initialized                          

      subroutine RPN_COMM_set_petopo(sizx,sizy)          
      implicit none                      
      integer, intent(IN) :: sizx,sizy   
      end subroutine RPN_COMM_set_petopo                 
      subroutine RPN_MPI_set_petopo(sizx,sizy)           
      implicit none                      
      integer, intent(IN) :: sizx,sizy   
      end subroutine RPN_MPI_set_petopo                  
      integer function RPN_COMM_petopo(pex,pey)          
      integer, intent(IN) :: pex,pey                     
      end function RPN_COMM_petopo                       

      integer function RPN_MPI_petopo(pex,pey)          
      implicit none                                      
      integer, intent(IN) :: pex,pey                     
      end function RPN_MPI_petopo                              

      integer function RPN_MPI_get_pe(x,y,grd,sgrd,communicator) 
      implicit none                                                
      integer, intent(IN) :: x,y,grd,sgrd                          
      character (len=*), intent(IN) :: communicator                   
      end function RPN_MPI_get_pe                              
      function RPN_MPI_set_timeout_alarm(seconds) result(seconds_since)  
      integer, intent(IN) :: seconds  
      integer :: seconds_since  
      end function RPN_MPI_set_timeout_alarm                             
      function RPN_COMM_set_timeout_alarm(seconds) result(seconds_since)  
      integer, intent(IN) :: seconds  
      integer :: seconds_since  
      end function RPN_COMM_set_timeout_alarm                             

      integer function RPN_COMM_topo_2(nxg,minx,maxx,nxl,nxlmax,halo,nx0,alongx,fill,relax,abort) 
      implicit none                                                         
      integer, intent(in) :: nxg,halo,relax                                 
      logical, intent(in) :: alongx,fill,abort                              
      integer, intent(out):: minx,maxx,nxl,nxlmax,nx0                       
      end function RPN_COMM_topo_2                                          

      integer function RPN_MPI_topology(nxg,minx,maxx,nxl,nxlmax,halo,nx0,alongx,fill,relax,abort) 
      implicit none                                                         
      integer, intent(in) :: nxg,halo,relax                                 
      logical, intent(in) :: alongx,fill,abort                              
      integer, intent(out):: minx,maxx,nxl,nxlmax,nx0                       
      end  function RPN_MPI_topology                     

      integer function RPN_COMM_topo(nxg,minx,maxx,nxl,nxlmax,halo,nx0,alongx,fill) 
      implicit none                                                                
      integer, intent(in) :: nxg,halo                                              
      logical, intent(in) :: alongx,fill                                           
      integer, intent(out):: minx,maxx,nxl,nxlmax,nx0                              
      end function RPN_COMM_topo                                                 
end interface
