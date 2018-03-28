!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- MODULE_LVL3_WRITE_OPERATIONS.f90 ----       **
!**                                                      **
!**     This module contains the following subroutines   **
!**     to write data into a file:                       **
!**       - ROUTINE_WRITE_VALUE_INTEGER                  **
!**       - ROUTINE_APPEND_VALUE_INTEGER                 **
!**       - ROUTINE_WRITE_VALUE_REAL                     **
!**       - ROUTINE_APPEND_VALUE_REAL                    **
!**       - ROUTINE_WRITE_VECTOR_INTEGER                 **
!**       - ROUTINE_WRITE_VECTOR_REAL                    **
!**       - ROUTINE_WRITE_VECTOR_REAL_SPECIAL            **
!**       - ROUTINE_WRITE_MATRIX_INTEGER                 **
!**       - ROUTINE_WRITE_MATRIX_INTEGER_01              **
!**       - ROUTINE_WRITE_MATRIX_REAL                    **
!**       - ROUTINE_APPEND_MATRIX_REAL                   **
!**       - ROUTINE_WRITE_MATRIX_REAL_CCS                **
!**       - ROUTINE_WRITE_MATRIX_REAL_TO_VECTOR          **
!**                                                      **
!**     This module uses the following modules:          **
!**       - MODULE_LVL1_KIND_NUMBERS                     **
!**       - MODULE_LVL2_CREATE_FORMATTINGS               **
!**       - MODULE_LVL2_ON_SCREEN_MESSAGES               **
!**                                                      **
!**     LEIBNIZ INSTITUTE (FBN)                          **
!**     Dummerstorf                                      **
!**     Start: 13 March 2018                             **
!**     Author: Jan Klosa                                **
!**                                                      **
!**********************************************************
!**********************************************************

module MODULE_LVL3_WRITE_OPERATIONS
  use MODULE_LVL1_KIND_NUMBERS
  use MODULE_LVL2_CREATE_FORMATTINGS
  use MODULE_LVL2_ON_SCREEN_MESSAGES
  implicit none
  
  contains
  
  
!**********************************************************
!**     Subroutine to write an integer into a .txt-      **
!**     file:                                            **
!**********************************************************
  subroutine ROUTINE_WRITE_VALUE_INTEGER(FILE_NAME, VALUE, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)       :: VALUE
    integer(ik4)                   :: io_error
    logical, intent(in)            :: SHOW_INFO
    character(len=40)              :: PLACE_ERROR
    character(len=200), intent(in) :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_WRITE_VALUE_INTEGER'
    
    open(21, file=FILE_NAME, action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    write(21, '(I20)', iostat=io_error) VALUE
    call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_WRITE_VALUE_INTEGER
  
  
!**********************************************************
!**     Subroutine to append an integer to an existing   **
!**     .txt-file:                                       **
!**********************************************************
  subroutine ROUTINE_APPEND_VALUE_INTEGER(FILE_NAME, VALUE, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)       :: VALUE
    integer(ik4)                   :: io_error
    logical, intent(in)            :: SHOW_INFO
    character(len=40)              :: PLACE_ERROR
    character(len=200), intent(in) :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_APPEND_VALUE_INTEGER'
    
    open(21, file=FILE_NAME, action='write', position='append', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    write(21, '(I20)', iostat=io_error) VALUE
    call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_APPEND_VALUE_INTEGER
  
  
!**********************************************************
!**     Subroutine to write a real value into a .txt-    **
!**     file:                                            **
!**********************************************************
  subroutine ROUTINE_WRITE_VALUE_REAL(FILE_NAME, VALUE, SHOW_INFO)
    implicit none
    
    integer(ik4)                   :: io_error
    real(rkdp), intent(in)         :: VALUE
    logical, intent(in)            :: SHOW_INFO
    character(len=40)              :: PLACE_ERROR
    character(len=200), intent(in) :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_WRITE_VALUE_REAL'
    
    open(21, file=FILE_NAME, action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    write(21, '(E24.16)', iostat=io_error) VALUE
    call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_WRITE_VALUE_REAL
  
  
!**********************************************************
!**     Subroutine to append a real value to an exis-    **
!**     ting .txt-file:                                  **
!**********************************************************
  subroutine ROUTINE_APPEND_VALUE_REAL(FILE_NAME, VALUE, SHOW_INFO)
    implicit none
    
    integer(ik4)                   :: io_error
    real(rkdp), intent(in)         :: VALUE
    logical, intent(in)            :: SHOW_INFO
    character(len=40)              :: PLACE_ERROR
    character(len=200), intent(in) :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_APPEND_VALUE_REAL'
    
    open(21, file=FILE_NAME, action='write', position='append', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    write(21, '(E24.16)', iostat=io_error) VALUE
    call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_APPEND_VALUE_REAL
  
  
!**********************************************************
!**     Subroutine to write an integer vector into a     **
!**     .txt-file:                                       **
!**********************************************************
  subroutine ROUTINE_WRITE_VECTOR_INTEGER(FILE_NAME, ROWS, VECTOR, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                  :: ROWS
    integer(ik4)                              :: i, io_error, alloc_error
    integer(ik4), dimension(ROWS), intent(in) :: VECTOR
    logical, intent(in)                       :: SHOW_INFO
    character(len=40)                         :: PLACE_ERROR
    character(len=200), intent(in)            :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_WRITE_VECTOR_INTEGER'
    
    open(21, file=FILE_NAME, action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    LOOP_ROWS: do i=1_ik4,ROWS
      write(21, '(I10)', iostat=io_error) VECTOR(i)
      call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_ROWS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_WRITE_VECTOR_INTEGER
  
  
!**********************************************************
!**     Subroutine to write a real-valued vector into    **
!**     a .txt-file:                                     **
!**********************************************************
  subroutine ROUTINE_WRITE_VECTOR_REAL(FILE_NAME, ROWS, VECTOR, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                :: ROWS
    integer(ik4)                            :: i, io_error, alloc_error
    real(rkdp), dimension(ROWS), intent(in) :: VECTOR
    logical, intent(in)                     :: SHOW_INFO
    character(len=40)                       :: PLACE_ERROR
    character(len=200), intent(in)          :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_WRITE_VECTOR_REAL'
    
    open(21, file=FILE_NAME, action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    LOOP_ROWS: do i=1_ik4,ROWS
      write(21, '(F20.16)', iostat=io_error) VECTOR(i)
      call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_ROWS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_WRITE_VECTOR_REAL
  
  
!**********************************************************
!**     Subroutine to enlarge and write a real-valued    **
!**     vector into a .txt-file:                         **
!**********************************************************
  subroutine ROUTINE_WRITE_VECTOR_REAL_SPECIAL(FILE_NAME, ENTIRE_ROWS, ROWS, iteration, VECTOR, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                            :: ENTIRE_ROWS, ROWS, iteration
    integer(ik4)                                        :: i, io_error
    real(rkdp)                                          :: DUMMY
    real(rkdp), dimension(ENTIRE_ROWS-ROWS), intent(in) :: VECTOR
    logical, intent(in)                                 :: SHOW_INFO
    character(len=40)                                   :: PLACE_ERROR
    character(len=200), intent(in)                      :: FILE_NAME
    
    DUMMY = -99999.0_rkdp
    PLACE_ERROR = 'ROUTINE_WRITE_VECTOR_REAL_SPECIAL'
    
    open(21, file=FILE_NAME, status='unknown', action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    LOOP_ROWS: do i=1_ik4,ENTIRE_ROWS
      CHECK_i: if (i .LE. (iteration-1_ik4)*ROWS) then
        write(21, '(1x, F20.12)', iostat=io_error) VECTOR(i)
        call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
      else if (i .GT. iteration*ROWS) then
        write(21, '(1x, F20.12)', iostat=io_error) VECTOR(i-ROWS)
        call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
      else
        write(21, '(1x, F20.12)', iostat=io_error) DUMMY
        call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
      end if CHECK_i
    end do LOOP_ROWS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_WRITE_VECTOR_REAL_SPECIAL
  
  
!**********************************************************
!**     Subroutine to write an integer matrix into a     **
!**     .txt-file:                                       **
!**********************************************************
  subroutine ROUTINE_WRITE_MATRIX_INTEGER(FILE_NAME, ROWS, COLUMNS, MATRIX, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                          :: ROWS, COLUMNS
    integer(ik4)                                      :: i, io_error, alloc_error
    integer(ik4), dimension(ROWS,COLUMNS), intent(in) :: MATRIX
    logical, intent(in)                               :: SHOW_INFO
    character, allocatable, dimension(:)              :: FORMATTING
    character(len=40)                                 :: NAME_VARIABLE, PLACE_ERROR
    character(len=200), intent(in)                    :: FILE_NAME
    
    NAME_VARIABLE = 'FORMATTING'
    PLACE_ERROR   = 'ROUTINE_WRITE_MATRIX_INTEGER'
    
    open(21, file=FILE_NAME, action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    allocate(FORMATTING(4_ik4*COLUMNS+1_ik4), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    call ROUTINE_CREATE_FORMATTING_INTEGER_I10(COLUMNS, FORMATTING)
    
    LOOP_ROWS: do i=1_ik4,ROWS
      write(21, FORMATTING, iostat=io_error) MATRIX(i,1_ik4:COLUMNS)
      call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_ROWS
    
    deallocate(FORMATTING, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_WRITE_MATRIX_INTEGER
  
  
!**********************************************************
!**     Subroutine to write an integer matrix which      **
!**     only contains Zeros and Ones into a .txt-file:   **
!**********************************************************
  subroutine ROUTINE_WRITE_MATRIX_INTEGER_01(FILE_NAME, ROWS, COLUMNS, MATRIX, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                          :: ROWS, COLUMNS
    integer(ik4)                                      :: i, io_error, alloc_error
    integer(ik4), dimension(ROWS,COLUMNS), intent(in) :: MATRIX
    logical, intent(in)                               :: SHOW_INFO
    character, allocatable, dimension(:)              :: FORMATTING
    character(len=40)                                 :: NAME_VARIABLE, PLACE_ERROR
    character(len=200), intent(in)                    :: FILE_NAME
    
    NAME_VARIABLE = 'FORMATTING'
    PLACE_ERROR   = 'ROUTINE_WRITE_MATRIX_INTEGER_01'
    
    open(21, file=FILE_NAME, action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    allocate(FORMATTING(3_ik4*COLUMNS+1_ik4), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    call ROUTINE_CREATE_FORMATTING_INTEGER_I2(COLUMNS, FORMATTING)
    
    LOOP_ROWS: do i=1_ik4,ROWS
      write(21, FORMATTING, iostat=io_error) MATRIX(i,1_ik4:COLUMNS)
      call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_ROWS
    
    deallocate(FORMATTING, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_WRITE_MATRIX_INTEGER_01
  
  
!**********************************************************
!**     Subroutine to write a real-valued matrix into    **
!**     a .txt-file:                                     **
!**********************************************************
  subroutine ROUTINE_WRITE_MATRIX_REAL(FILE_NAME, ROWS, COLUMNS, MATRIX, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                        :: ROWS, COLUMNS
    integer(ik4)                                    :: i, io_error, alloc_error
    real(rkdp), dimension(ROWS,COLUMNS), intent(in) :: MATRIX
    logical, intent(in)                             :: SHOW_INFO
    character, allocatable, dimension(:)            :: FORMATTING
    character(len=40)                               :: NAME_VARIABLE, PLACE_ERROR
    character(len=200), intent(in)                  :: FILE_NAME
    
    NAME_VARIABLE = 'FORMATTING'
    PLACE_ERROR   = 'ROUTINE_WRITE_MATRIX_REAL'
    
    open(21, file=FILE_NAME, action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    allocate(FORMATTING(7_ik4*COLUMNS+1_ik4), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    call ROUTINE_CREATE_FORMATTING_REAL_E2616(COLUMNS, FORMATTING)
    
    LOOP_ROWS: do i=1_ik4,ROWS
      write(21, FORMATTING, iostat=io_error) MATRIX(i,1_ik4:COLUMNS)
      call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_ROWS
    
    deallocate(FORMATTING, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_WRITE_MATRIX_REAL
  
  
!**********************************************************
!**     Subroutine to append a real-valued matrix to     **
!**     an existing .txt-file:                           **
!**********************************************************
  subroutine ROUTINE_APPEND_MATRIX_REAL(FILE_NAME, ROWS, COLUMNS, MATRIX, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                        :: ROWS, COLUMNS
    integer(ik4)                                    :: i, io_error, alloc_error
    real(rkdp), dimension(ROWS,COLUMNS), intent(in) :: MATRIX
    logical, intent(in)                             :: SHOW_INFO
    character, allocatable, dimension(:)            :: FORMATTING
    character(len=40)                               :: NAME_VARIABLE, PLACE_ERROR
    character(len=200), intent(in)                  :: FILE_NAME
    
    NAME_VARIABLE = 'FORMATTING'
    PLACE_ERROR   = 'ROUTINE_APPEND_MATRIX_REAL'
    
    open(21, file=FILE_NAME, action='write', position='append', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    allocate(FORMATTING(7_ik4*COLUMNS+1_ik4), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    call ROUTINE_CREATE_FORMATTING_REAL_E2616(COLUMNS, FORMATTING)
    
    LOOP_ROWS: do i=1_ik4,ROWS
      write(21, FORMATTING, iostat=io_error) MATRIX(i,1_ik4:COLUMNS)
      call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_ROWS
    
    deallocate(FORMATTING, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_APPEND_MATRIX_REAL
  
  
!**********************************************************
!**     Subroutine to write a real-valued matrix into    **
!**     an .mm-file using compressed column storage      **
!**     (CCS) matrix format:                             **
!**********************************************************
  subroutine ROUTINE_WRITE_MATRIX_REAL_CCS(FILE_NAME, ROWS, COLUMNS, MATRIX, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                        :: ROWS, COLUMNS
    integer(ik4)                                    :: i, j, COUNTER, io_error, alloc_error
    real(rkdp), dimension(ROWS,COLUMNS), intent(in) :: MATRIX
    logical, intent(in)                             :: SHOW_INFO
    character(len=40)                               :: PLACE_ERROR, DUMMY_STRING_1, DUMMY_STRING_2, DUMMY_STRING_3
    character(len=200), intent(in)                  :: FILE_NAME
    
    COUNTER        = 0_ik4
    PLACE_ERROR    = 'ROUTINE_WRITE_MATRIX_REAL_CCS'
    DUMMY_STRING_1 = ''
    DUMMY_STRING_2 = ''
    DUMMY_STRING_3 = ''
    
    open(21, file=FILE_NAME, action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    write(21, '(A)', iostat=io_error) '%%MatrixMarket matrix coordinate real general'
    
    write(DUMMY_STRING_1, '(I40)') ROWS
    DUMMY_STRING_1 = adjustl(DUMMY_STRING_1)
    write(DUMMY_STRING_2, '(I40)') COLUMNS
    DUMMY_STRING_2 = adjustl(DUMMY_STRING_2)
    LOOP_ROWS_1: do i=1_ik4,ROWS
      LOOP_COLUMNS_1: do j=1_ik4,COLUMNS
        CHECK_ENTRY_1: if (MATRIX(i,j) .NE. 0.0_rkdp) then
          COUNTER = COUNTER+1_ik4
        end if CHECK_ENTRY_1
      end do LOOP_COLUMNS_1
    end do LOOP_ROWS_1
    write(DUMMY_STRING_3, '(I40)') COUNTER
    DUMMY_STRING_3 = adjustl(DUMMY_STRING_3)
    DUMMY_STRING_1 = trim(DUMMY_STRING_1)//' '//trim(DUMMY_STRING_2)//' '//trim(DUMMY_STRING_3)
    
    write(21, '(A)', iostat=io_error) trim(adjustl(DUMMY_STRING_1))
    
    LOOP_COLUMNS_2: do j=1_ik4,COLUMNS
      LOOP_ROWS_2: do i=1_ik4,ROWS
        CHECK_ENTRY_2: if (MATRIX(i,j) .NE. 0.0_rkdp) then
          write(DUMMY_STRING_1, '(I40)') i
          DUMMY_STRING_1 = adjustl(DUMMY_STRING_1)
          write(DUMMY_STRING_2, '(I40)') j
          DUMMY_STRING_2 = adjustl(DUMMY_STRING_2)
          write(DUMMY_STRING_3, '(E40.16)') MATRIX(i,j)
          DUMMY_STRING_3 = adjustl(DUMMY_STRING_3)
          DUMMY_STRING_1 = trim(DUMMY_STRING_1)//' '//trim(DUMMY_STRING_2)//' '//trim(DUMMY_STRING_3)
          
          write(21, '(A)', iostat=io_error) trim(adjustl(DUMMY_STRING_1))
          call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
        end if CHECK_ENTRY_2
      end do LOOP_ROWS_2
    end do LOOP_COLUMNS_2
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_WRITE_MATRIX_REAL_CCS
  
  
!**********************************************************
!**     Subroutine to write certain values of a real-    **
!**     valued matrix as a vector into a .txt-file:      **
!**********************************************************
  subroutine ROUTINE_WRITE_MATRIX_REAL_TO_VECTOR(FILE_NAME, ROWS, COLUMNS, WINDOW_SIZE, MATRIX, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                        :: ROWS, COLUMNS, WINDOW_SIZE
    integer(ik4)                                    :: i, j, io_error
    real(rkdp), dimension(ROWS,COLUMNS), intent(in) :: MATRIX
    logical, intent(in)                             :: SHOW_INFO
    character(len=40)                               :: PLACE_ERROR
    character(len=200), intent(in)                  :: FILE_NAME
    
    PLACE_ERROR   = 'ROUTINE_WRITE_MATRIX_REAL_TO_VECTOR'
    
    open(21, file=FILE_NAME, action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    LOOP_ROWS: do i=1_ik4,ROWS-WINDOW_SIZE
      LOOP_COLUMNS: do j=i+1_ik4,i+WINDOW_SIZE
        write(21, '(F20.16)', iostat=io_error) MATRIX(i,j)
        call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
      end do LOOP_COLUMNS
    end do LOOP_ROWS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_WRITE(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_WRITE_MATRIX_REAL_TO_VECTOR
end module MODULE_LVL3_WRITE_OPERATIONS
