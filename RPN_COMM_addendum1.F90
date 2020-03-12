module RPN_COMM_addendum
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: RPN_COMM_get_my_domain,RPN_COMM_gridinit

  interface RPN_COMM_get_my_domain
      module procedure RPN_COMM_get_my_domain_1
      module procedure RPN_COMM_get_my_domain_2
  end interface RPN_COMM_get_my_domain
  interface RPN_COMM_gridinit
      module procedure RPN_COMM_init_multi_level1
      module procedure RPN_COMM_init_multi_level2
  end interface RPN_COMM_gridinit

contains
      
function RPN_COMM_get_my_domain_1(call_back) result (domain) !InTf!
   implicit none
   external :: call_back !InTf!
   integer :: domain !InTf!
   integer :: temp
   call RPN_COMM_mydomain (call_back, temp)
   domain = temp
   return
end function RPN_COMM_get_my_domain_1 !InTf!

function RPN_COMM_get_my_domain_2(ndomains, offset) result (domain) !InTf!
   implicit none
   integer, intent(IN) :: ndomains, offset !InTf!
   integer :: domain !InTf!
   integer, save :: local_ndomains, local_offset
   integer :: temp
   local_ndomains = ndomains
   local_offset = offset
   call RPN_COMM_mydomain (internal, temp)
   domain = temp
   return

contains
   subroutine internal(ndomains, offset, err)
     integer, intent(OUT) :: ndomains, offset, err
     ndomains = local_ndomains
     offset = local_offset
      err=0
     return
   end subroutine internal
end function RPN_COMM_get_my_domain_2 !InTf!

SUBROUTINE rpn_comm_dummy_init(Pex,Pey)
   implicit none
   integer, intent(inout) :: Pex,Pey
   Pex = Pex
   Pey = Pey
   return
end SUBROUTINE rpn_comm_dummy_init

FUNCTION RPN_COMM_init1(Userinit,Pelocal,Petotal) result(grid) !InTf!
   implicit none
   external Userinit !InTf!
   integer, intent(out)   :: Pelocal,Petotal !InTf!
   integer :: grid !InTf!
   integer :: Pex,Pey
   Pex = 0
   Pey = 0
   call RPN_COMM_init(Userinit,Pelocal,Petotal,Pex,Pey)
   grid = 0
   return
end FUNCTION RPN_COMM_init1 !InTf!

FUNCTION RPN_COMM_init2(Pelocal,Petotal,Pex,Pey) result(grid) !InTf!
   implicit none
   integer, intent(out)   :: Pelocal,Petotal !InTf!
   integer, intent(inout) :: Pex,Pey !InTf!
   integer :: grid !InTf!
   call RPN_COMM_init(rpn_comm_dummy_init,Pelocal,Petotal,Pex,Pey)
   grid = 0
   return
end FUNCTION RPN_COMM_init2 !InTf!

FUNCTION RPN_COMM_init_multigrid1(Userinit,Pelocal,Petotal,MultiGrids) result(grid)  !InTf!
   implicit none
   external Userinit !InTf!
   integer, intent(out)   ::Pelocal,Petotal                                          !InTf!
   integer, intent(in)    ::MultiGrids                                               !InTf!
   integer :: grid !InTf!
   integer :: Pex,Pey
   integer, external :: RPN_COMM_init_multigrid
   Pex = 0
   Pey = 0
   grid =RPN_COMM_init_multigrid(Userinit,Pelocal,Petotal,Pex,Pey,MultiGrids)
end FUNCTION RPN_COMM_init_multigrid1 !InTf!

FUNCTION RPN_COMM_init_multigrid2(Pelocal,Petotal,Pex,Pey,MultiGrids) result(grid)   !InTf!
   implicit none
   integer, intent(out)   ::Pelocal,Petotal                                          !InTf!
   integer, intent(inout) ::Pex,Pey                                                  !InTf!
   integer, intent(in)    ::MultiGrids                                               !InTf!
   integer :: grid !InTf!
   integer, external :: RPN_COMM_init_multigrid
   grid =RPN_COMM_init_multigrid(rpn_comm_dummy_init,Pelocal,Petotal,Pex,Pey,MultiGrids)
end FUNCTION RPN_COMM_init_multigrid2 !InTf!

FUNCTION RPN_COMM_init_multi_level1(Userinit,Pelocal,Petotal,MultiGrids,Grids) result(grid)  !InTf!
   implicit none
   external Userinit !InTf!
   integer, intent(out)   :: Pelocal,Petotal !InTf!
   integer, intent(in)    :: MultiGrids !InTf!
   integer, intent(in)    :: Grids !InTf!
   integer :: grid !InTf!
   integer :: Pex,Pey
   integer, external :: RPN_COMM_init_multi_level
   Pex = 0
   Pey = 0
   grid =RPN_COMM_init_multi_level(Userinit,Pelocal,Petotal,Pex,Pey,MultiGrids,Grids)
end FUNCTION RPN_COMM_init_multi_level1 !InTf!

FUNCTION RPN_COMM_init_multi_level2(Pelocal,Petotal,Pex,Pey,MultiGrids,Grids) result(grid)   !InTf!
   implicit none
   integer, intent(out)   :: Pelocal,Petotal !InTf!
   integer, intent(inout) :: Pex,Pey !InTf!
   integer, intent(in)    :: MultiGrids !InTf!
   integer, intent(in)    :: Grids !InTf!
   integer :: grid !InTf!
   integer, external :: RPN_COMM_init_multi_level
   grid =RPN_COMM_init_multi_level(rpn_comm_dummy_init,Pelocal,Petotal,Pex,Pey,MultiGrids,Grids)
end FUNCTION RPN_COMM_init_multi_level2 !InTf!

end module RPN_COMM_addendum
