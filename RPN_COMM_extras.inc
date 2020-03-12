interface RPN_COMM_get_my_domain                             !InTf!
function RPN_COMM_get_my_domain_1(call_back) result (domain)    !InTf!
  external :: call_back                                         !InTf!
  integer :: domain                                             !InTf!
end function RPN_COMM_get_my_domain_1                           !InTf!
function RPN_COMM_get_my_domain_2(ndomains, offset) result (domain) !InTf!
  integer, intent(IN) :: ndomains, offset                       !InTf!
  integer :: domain                                             !InTf!
end function RPN_COMM_get_my_domain_2                           !InTf!
end interface RPN_COMM_get_my_domain                         !InTf!
interface RPN_COMM_gridinit           !InTf!
FUNCTION RPN_COMM_init1(Userinit,Pelocal,Petotal) result(grid)   !InTf!
  external Userinit                                              !InTf!
  integer, intent(out)   :: Pelocal,Petotal                      !InTf!
  integer :: grid                                                !InTf!
end FUNCTION RPN_COMM_init1                                      !InTf!
FUNCTION RPN_COMM_init2(Pelocal,Petotal,Pex,Pey) result(grid)    !InTf!
  integer, intent(out)   :: Pelocal,Petotal                      !InTf!
  integer, intent(inout) :: Pex,Pey                              !InTf!
  integer :: grid                                                !InTf!
end FUNCTION RPN_COMM_init2                                      !InTf!
FUNCTION RPN_COMM_init_multigrid1(Userinit,Pelocal,Petotal,MultiGrids) result(grid)  !InTf!
  external Userinit                                                                  !InTf!
  integer, intent(out)   :: Pelocal,Petotal                                          !InTf!
  integer, intent(in)    :: MultiGrids                                               !InTf!
  integer :: grid                                                                    !InTf!
end FUNCTION RPN_COMM_init_multigrid1                                                !InTf!
FUNCTION RPN_COMM_init_multigrid2(Pelocal,Petotal,Pex,Pey,MultiGrids) result(grid)   !InTf!
  integer, intent(out)   :: Pelocal,Petotal                                          !InTf!
  integer, intent(inout) :: Pex,Pey                                                  !InTf!
  integer, intent(in)    :: MultiGrids                                               !InTf!
  integer :: grid                                                                    !InTf!
end FUNCTION RPN_COMM_init_multigrid2                                                !InTf!
FUNCTION RPN_COMM_init_multi_level1(Userinit,Pelocal,Petotal,MultiGrids,Grids) result(grid)  !InTf!
  external Userinit                                                                          !InTf!
  integer, intent(out)   :: Pelocal,Petotal                                                  !InTf!
  integer, intent(in)    :: MultiGrids                                                       !InTf!
  integer, intent(in)    :: Grids                                                            !InTf!
  integer :: grid                                                                            !InTf!
end FUNCTION RPN_COMM_init_multi_level1                                                      !InTf!
FUNCTION RPN_COMM_init_multi_level2(Pelocal,Petotal,Pex,Pey,MultiGrids,Grids) result(grid)   !InTf!
  integer, intent(out)   :: Pelocal,Petotal                                                  !InTf!
  integer, intent(inout) :: Pex,Pey                                                          !InTf!
  integer, intent(in)    :: MultiGrids                                                       !InTf!
  integer, intent(in)    :: Grids                                                            !InTf!
  integer :: grid                                                                            !InTf!
end FUNCTION RPN_COMM_init_multi_level2                                                      !InTf!
end interface RPN_COMM_gridinit       !InTf!
