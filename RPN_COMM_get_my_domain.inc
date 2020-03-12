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
