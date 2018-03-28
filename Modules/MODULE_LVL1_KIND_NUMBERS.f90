!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- MODULE_LVL1_KIND_NUMBERS.f90 ----           **
!**                                                      **
!**     This module specifies parameters for numerical   **
!**     precision.                                       **
!**                                                      **
!**     LEIBNIZ INSTITUTE (FBN)                          **
!**     Dummerstorf                                      **
!**     Start: 13 March 2018                             **
!**     Author: Jan Klosa                                **
!**                                                      **
!**********************************************************
!**********************************************************

module MODULE_LVL1_KIND_NUMBERS
  implicit none
  
  integer, parameter :: ik0  = selected_int_kind(1)
  integer, parameter :: ik2  = selected_int_kind(4)
  integer, parameter :: ik4  = selected_int_kind(9)
  integer, parameter :: ikxl = selected_int_kind(12)
  integer, parameter :: rksp = selected_real_kind(6,37)
  integer, parameter :: rkdp = selected_real_kind(15,307)
  integer, parameter :: rkqp = selected_real_kind(33,4931)
end module MODULE_LVL1_KIND_NUMBERS
