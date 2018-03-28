!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- MODULE_LVL2_CREATE_FORMATTINGS.f90 ----     **
!**                                                      **
!**     This module contains the following subroutines   **
!**     to create output formattings:                    **
!**       - ROUTINE_CREATE_FORMATTING_REAL_F2016         **
!**       - ROUTINE_CREATE_FORMATTING_REAL_F4016         **
!**       - ROUTINE_CREATE_FORMATTING_INTEGER_I2         **
!**       - ROUTINE_CREATE_FORMATTING_INTEGER_I10        **
!**                                                      **
!**     This module uses the following modules:          **
!**       - MODULE_LVL1_KIND_NUMBERS                     **
!**                                                      **
!**     LEIBNIZ INSTITUTE (FBN)                          **
!**     Dummerstorf                                      **
!**     Start: 13 March 2018                             **
!**     Author: Jan Klosa                                **
!**                                                      **
!**********************************************************
!**********************************************************

module MODULE_LVL2_CREATE_FORMATTINGS
  use MODULE_LVL1_KIND_NUMBERS
  implicit none
  
  contains
  
  
!**********************************************************
!**     Subroutine to create an output formatting for    **
!**     a real-valued output of the form F20.16:         **
!**********************************************************
  subroutine ROUTINE_CREATE_FORMATTING_REAL_F2016(COLUMNS, FORMATTING)
    implicit none
    
    integer(ik4), intent(in)                                 :: COLUMNS
    integer(ik4)                                             :: i
    character, dimension(7_ik4*COLUMNS+1_ik4), intent(inout) :: FORMATTING
    
    FORMATTING(1_ik4) = '('
    FORMATTING(2_ik4) = 'F'
    FORMATTING(3_ik4) = '2'
    FORMATTING(4_ik4) = '0'
    FORMATTING(5_ik4) = '.'
    FORMATTING(6_ik4) = '1'
    FORMATTING(7_ik4) = '6'
    do i=1_ik4,COLUMNS-1_ik4
      FORMATTING(7_ik4*i+1_ik4) = ','
      FORMATTING(7_ik4*i+2_ik4) = 'F'
      FORMATTING(7_ik4*i+3_ik4) = '2'
      FORMATTING(7_ik4*i+4_ik4) = '0'
      FORMATTING(7_ik4*i+5_ik4) = '.'
      FORMATTING(7_ik4*i+6_ik4) = '1'
      FORMATTING(7_ik4*i+7_ik4) = '6'
    end do
    FORMATTING(7_ik4*COLUMNS+1_ik4) = ')'
  end subroutine ROUTINE_CREATE_FORMATTING_REAL_F2016
  
  
!**********************************************************
!**     Subroutine to create an output formatting for    **
!**     a real-valued output of the form E26.16:         **
!**********************************************************
  subroutine ROUTINE_CREATE_FORMATTING_REAL_E2616(COLUMNS, FORMATTING)
    implicit none
    
    integer(ik4), intent(in)                                 :: COLUMNS
    integer(ik4)                                             :: i
    character, dimension(7_ik4*COLUMNS+1_ik4), intent(inout) :: FORMATTING
    
    FORMATTING(1_ik4) = '('
    FORMATTING(2_ik4) = 'E'
    FORMATTING(3_ik4) = '2'
    FORMATTING(4_ik4) = '6'
    FORMATTING(5_ik4) = '.'
    FORMATTING(6_ik4) = '1'
    FORMATTING(7_ik4) = '6'
    do i=1_ik4,COLUMNS-1_ik4
      FORMATTING(7_ik4*i+1_ik4) = ','
      FORMATTING(7_ik4*i+2_ik4) = 'E'
      FORMATTING(7_ik4*i+3_ik4) = '2'
      FORMATTING(7_ik4*i+4_ik4) = '6'
      FORMATTING(7_ik4*i+5_ik4) = '.'
      FORMATTING(7_ik4*i+6_ik4) = '1'
      FORMATTING(7_ik4*i+7_ik4) = '6'
    end do
    FORMATTING(7_ik4*COLUMNS+1_ik4) = ')'
  end subroutine ROUTINE_CREATE_FORMATTING_REAL_E2616
  
  
!**********************************************************
!**     Subroutine to create an output formatting for    **
!**     a real-valued output of the form F40.16:         **
!**********************************************************
  subroutine ROUTINE_CREATE_FORMATTING_REAL_F4016(COLUMNS, FORMATTING)
    implicit none
    
    integer(ik4), intent(in)                                 :: COLUMNS
    integer(ik4)                                             :: i
    character, dimension(7_ik4*COLUMNS+1_ik4), intent(inout) :: FORMATTING
    
    FORMATTING(1_ik4) = '('
    FORMATTING(2_ik4) = 'F'
    FORMATTING(3_ik4) = '4'
    FORMATTING(4_ik4) = '0'
    FORMATTING(5_ik4) = '.'
    FORMATTING(6_ik4) = '1'
    FORMATTING(7_ik4) = '6'
    do i=1_ik4,COLUMNS-1_ik4
      FORMATTING(7_ik4*i+1_ik4) = ','
      FORMATTING(7_ik4*i+2_ik4) = 'F'
      FORMATTING(7_ik4*i+3_ik4) = '4'
      FORMATTING(7_ik4*i+4_ik4) = '0'
      FORMATTING(7_ik4*i+5_ik4) = '.'
      FORMATTING(7_ik4*i+6_ik4) = '1'
      FORMATTING(7_ik4*i+7_ik4) = '6'
    end do
    FORMATTING(7_ik4*COLUMNS+1_ik4) = ')'
  end subroutine ROUTINE_CREATE_FORMATTING_REAL_F4016
  
  
!**********************************************************
!**     Subroutine to create an output formatting for    **
!**     an integer output of the form I2:                **
!**********************************************************
  subroutine ROUTINE_CREATE_FORMATTING_INTEGER_I2(COLUMNS, FORMATTING)
    implicit none
    
    integer(ik4), intent(in)                                 :: COLUMNS
    integer(ik4)                                             :: i
    character, dimension(3_ik4*COLUMNS+1_ik4), intent(inout) :: FORMATTING
    
    FORMATTING(1_ik4) = '('
    FORMATTING(2_ik4) = 'I'
    FORMATTING(3_ik4) = '2'
    do i=1,COLUMNS-1
      FORMATTING(3_ik4*i+1_ik4) = ','
      FORMATTING(3_ik4*i+2_ik4) = 'I'
      FORMATTING(3_ik4*i+3_ik4) = '2'
    end do
    FORMATTING(3_ik4*COLUMNS+1_ik4) = ')'
  end subroutine ROUTINE_CREATE_FORMATTING_INTEGER_I2
  
  
!**********************************************************
!**     Subroutine to create an output formatting for    **
!**     an integer output of the form I10:               **
!**********************************************************
  subroutine ROUTINE_CREATE_FORMATTING_INTEGER_I10(COLUMNS, FORMATTING)
    implicit none
    
    integer(ik4), intent(in)                                 :: COLUMNS
    integer(ik4)                                             :: i
    character, dimension(4_ik4*COLUMNS+1_ik4), intent(inout) :: FORMATTING
    
    FORMATTING(1_ik4) = '('
    FORMATTING(2_ik4) = 'I'
    FORMATTING(3_ik4) = '1'
    FORMATTING(4_ik4) = '0'
    do i=1,COLUMNS-1
      FORMATTING(4_ik4*i+1_ik4) = ','
      FORMATTING(4_ik4*i+2_ik4) = 'I'
      FORMATTING(4_ik4*i+3_ik4) = '1'
      FORMATTING(4_ik4*i+4_ik4) = '0'
    end do
    FORMATTING(4_ik4*COLUMNS+1_ik4) = ')'
  end subroutine ROUTINE_CREATE_FORMATTING_INTEGER_I10
end module MODULE_LVL2_CREATE_FORMATTINGS
