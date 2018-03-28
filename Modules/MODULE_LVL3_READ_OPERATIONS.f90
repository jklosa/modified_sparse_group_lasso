!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- MODULE_LVL3_READ_OPERATIONS.f90 ----        **
!**                                                      **
!**     This module contains the following subroutines   **
!**     for any read instruction:                        **
!**       - ROUTINE_DETERMINE_DIMENSION                  **
!**       - ROUTINE_READ_MATRIX_INTEGER                  **
!**       - ROUTINE_READ_MATRIX_REAL                     **
!**       - ROUTINE_READ_MATRIX_REAL_ROW_COLUMN_ENTRY    **
!**       - ROUTINE_READ_MATRIX_REAL_CCS                 **
!**       - ROUTINE_READ_COLUMN_INTEGER                  **
!**       - ROUTINE_READ_COLUMN_REAL                     **
!**       - ROUTINE_READ_ROW_INTEGER                     **
!**       - ROUTINE_READ_ROW_REAL                        **
!**       - ROUTINE_READ_INTEGER                         **
!**       - ROUTINE_READ_REAL                            **
!**                                                      **
!**     This module uses the following modules:          **
!**       - MODULE_LVL1_KIND_NUMBERS                     **
!**       - MODULE_LVL2_ON_SCREEN_MESSAGES               **
!**                                                      **
!**     LEIBNIZ INSTITUTE (FBN)                          **
!**     Dummerstorf                                      **
!**     Start: 13 March 2018                             **
!**     Author: Jan Klosa                                **
!**                                                      **
!**********************************************************
!**********************************************************

module MODULE_LVL3_READ_OPERATIONS
  use MODULE_LVL1_KIND_NUMBERS
  use MODULE_LVL2_ON_SCREEN_MESSAGES
  implicit none
  
  contains
  
  
!**********************************************************
!**     Subroutine to determine the number of rows       **
!**     and the number of columns of a numerical file:   **
!**********************************************************
  subroutine ROUTINE_DETERMINE_DIMENSION(FILE_NAME, ROWS, COLUMNS, SKIP_ROWS, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)       :: SKIP_ROWS
    integer(ik4), intent(inout)    :: ROWS, COLUMNS
    integer(ik4)                   :: LENGTH_NEW, i, io_error
    logical, intent(in)            :: SHOW_INFO
    logical                        :: SPACE, EMPTY
    character(len=40)              :: PLACE_ERROR
    character(len=200), intent(in) :: FILE_NAME
    character(len=200000)          :: TEMP_STRING
    
    ROWS        = 0_ik4
    COLUMNS     = 0_ik4
    LENGTH_NEW  = 0_ik4
    EMPTY       = .FALSE.
    PLACE_ERROR = 'ROUTINE_DETERMINE_DIMENSION'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    !******************************************************
    !**     Skip the first rows, if necessary:           **
    !******************************************************
    CHECK_SKIP: if (SKIP_ROWS .GT. 0_ik4) then
      LOOP_SKIP: do i=1_ik4,SKIP_ROWS
        read(21, *, iostat=io_error)
        call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
      end do LOOP_SKIP
    end if CHECK_SKIP
    
    !******************************************************
    !**     Read the first needed row as a string:       **
    !******************************************************
    read(21, '(A)', iostat=io_error) TEMP_STRING
    CHECK_IO_ERROR: if (io_error .LT. 0_ik4) then
      CHECK_SHOW_INFO: if (SHOW_INFO) then
        call ROUTINE_INFO_READ_EMPTY_FILE(FILE_NAME)
      end if CHECK_SHOW_INFO
      EMPTY = .TRUE.
      close(21, iostat=io_error)
      call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    else if (io_error .GT. 0_ik4) then
      call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
      EMPTY = .TRUE.
    end if CHECK_IO_ERROR
    TEMP_STRING = adjustl(TEMP_STRING)
    SPACE       = .FALSE.
    LENGTH_NEW  = len_trim(TEMP_STRING)
    
    !******************************************************
    !**     Count the words inside this string:          **
    !**     (number of words = number of columns)        **
    !******************************************************
    CHECK_EMPTY: if (LENGTH_NEW .GT. 0_ik4) then
      COLUMNS = 1_ik4
      LOOP_ENTRIES: do i=1_ik4,LENGTH_NEW
        CHECK_ENTRY: if ((TEMP_STRING(i:i) .EQ. ' ') .OR. (TEMP_STRING(i:i) .EQ. '	')) then
          SPACE = .TRUE.
        else
          CHECK_SPACE: if (SPACE) then
            COLUMNS = COLUMNS+1_ik4
          end if CHECK_SPACE
          SPACE = .FALSE.
        end if CHECK_ENTRY
      end do LOOP_ENTRIES
    end if CHECK_EMPTY
    
    !******************************************************
    !**     Determine the number of rows:                **
    !******************************************************
    CHECK_EMPTY_FILE: if (EMPTY) then
      ROWS = 0_ik4
    else
      ROWS = ROWS+1_ik4
      
      LOOP_ROWS: do
        read(21, '(A)', iostat=io_error) TEMP_STRING
        CHECK_STATUS: if (io_error .NE. 0_ik4) then
          exit LOOP_ROWS
        end if CHECK_STATUS
        ROWS = ROWS+1_ik4
      end do LOOP_ROWS
      
      close(21, iostat=io_error)
      call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    end if CHECK_EMPTY_FILE
  end subroutine ROUTINE_DETERMINE_DIMENSION
  
  
!**********************************************************
!**     Subroutine to read in an integer matrix of       **
!**     standard format (n x m) from a .txt-file:        **
!**********************************************************
  subroutine ROUTINE_READ_MATRIX_INTEGER(FILE_NAME, ROWS, COLUMNS, SKIP_ROWS, MATRIX, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                             :: ROWS, COLUMNS, SKIP_ROWS
    integer(ik4)                                         :: i, io_error
    integer(ik4), dimension(ROWS,COLUMNS), intent(inout) :: MATRIX
    logical, intent(in)                                  :: SHOW_INFO
    character(len=40)                                    :: PLACE_ERROR
    character(len=200), intent(in)                       :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_READ_MATRIX_INTEGER'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SKIP: if (SKIP_ROWS .GT. 0_ik4) then
      LOOP_SKIP: do i=1_ik4,SKIP_ROWS
        read(21, *, iostat=io_error)
        call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
      end do LOOP_SKIP
    end if CHECK_SKIP
    
    LOOP_ROWS: do i=1_ik4,ROWS
      read(21, *) MATRIX(i,1_ik4:COLUMNS)
    end do LOOP_ROWS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_READ(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_READ_MATRIX_INTEGER
  
  
!**********************************************************
!**     Subroutine to read in a real-valued matrix of    **
!**     standard format (n x m) from a .txt-file:        **
!**********************************************************
  subroutine ROUTINE_READ_MATRIX_REAL(FILE_NAME, ROWS, COLUMNS, SKIP_ROWS, MATRIX, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                           :: ROWS, COLUMNS, SKIP_ROWS
    integer(ik4)                                       :: i, io_error
    real(rkdp), dimension(ROWS,COLUMNS), intent(inout) :: MATRIX
    logical, intent(in)                                :: SHOW_INFO
    character(len=40)                                  :: PLACE_ERROR
    character(len=200), intent(in)                     :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_READ_MATRIX_REAL'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SKIP: if (SKIP_ROWS .GT. 0_ik4) then
      LOOP_SKIP: do i=1_ik4,SKIP_ROWS
        read(21, *, iostat=io_error)
        call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
      end do LOOP_SKIP
    end if CHECK_SKIP
    
    LOOP_ROWS: do i=1_ik4,ROWS
      read(21, *) MATRIX(i,1_ik4:COLUMNS)
    end do LOOP_ROWS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_READ(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_READ_MATRIX_REAL
  
  
!**********************************************************
!**     Subroutine to read in a real-valued symmetric    **
!**     matrix (lower triangle part) of special format   **
!**     (row, column, entry):                            **
!**********************************************************
  subroutine ROUTINE_READ_MATRIX_REAL_ROW_COLUMN_ENTRY(FILE_NAME, ROWS, SKIP_ROWS, MATRIX, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                        :: ROWS, SKIP_ROWS
    integer(ik4)                                    :: i, j, k, io_error
    real(rkdp), dimension(ROWS,ROWS), intent(inout) :: MATRIX
    logical, intent(in)                             :: SHOW_INFO
    character(len=40)                               :: PLACE_ERROR
    character(len=200), intent(in)                  :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_READ_MATRIX_ROW_COLUMN_ENTRY'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SKIP: if (SKIP_ROWS .GT. 0_ik4) then
      LOOP_SKIP: do i=1_ik4,SKIP_ROWS
        read(21, *, iostat=io_error)
        call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
      end do LOOP_SKIP
    end if CHECK_SKIP
    
    LOOP_ROWS: do k = 1_ik4,((ROWS*(ROWS+1_ik4))/2_ik4)
      read(21, *, iostat=io_error) i, j, MATRIX(i,j)
      call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
      MATRIX(j,i) = MATRIX(i,j)
    end do LOOP_ROWS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_READ(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_READ_MATRIX_REAL_ROW_COLUMN_ENTRY
  
  
!**********************************************************
!**     Subroutine to read in a real-valued matrix       **
!**     stored in compressed column storage (CCS) matrix **
!**     format in an .mm-file:                           **
!**********************************************************
  subroutine ROUTINE_READ_MATRIX_REAL_CCS(FILE_NAME, ROWS, MATRIX, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                        :: ROWS
    integer(ik4)                                    :: i, j, k, END_LOOP, io_error
    real(rkdp)                                      :: TEMP
    real(rkdp), dimension(ROWS,ROWS), intent(inout) :: MATRIX
    logical, intent(in)                             :: SHOW_INFO
    character(len=40)                               :: PLACE_ERROR
    character(len=200), intent(in)                  :: FILE_NAME
    
    END_LOOP    = 0_ik4
    temp        = 0.0_rkdp
    PLACE_ERROR = 'ROUTINE_READ_MATRIX_REAL_CCS'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    read(21, *, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
    read(21, *, iostat=io_error), i, j, END_LOOP
    call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
    
    LOOP_ROWS: do k = 1_ik4,END_LOOP
      read(21, *, iostat=io_error) i, j, TEMP
      call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
      MATRIX(i,j) = TEMP
    end do LOOP_ROWS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_READ(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_READ_MATRIX_REAL_CCS
  
  
!**********************************************************
!**     Subroutine to read in a certain column of        **
!**     integers from a .txt-file:                       **
!**********************************************************
  subroutine ROUTINE_READ_COLUMN_INTEGER(FILE_NAME, ROWS, COLUMN, SKIP_ROWS, VECTOR, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                     :: ROWS, COLUMN, SKIP_ROWS
    integer(ik4)                                 :: i, io_error
    integer(ik4), dimension(ROWS), intent(inout) :: VECTOR
    integer(ik4), dimension(COLUMN-1_ik4)        :: TEMP
    logical, intent(in)                          :: SHOW_INFO
    character(len=40)                            :: PLACE_ERROR
    character(len=200), intent(in)               :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_READ_COLUMN_INTEGER'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SKIP: if (SKIP_ROWS .GT. 0_ik4) then
      LOOP_SKIP: do i=1_ik4,SKIP_ROWS
        read(21, *, iostat=io_error)
        call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
      end do LOOP_SKIP
    end if CHECK_SKIP
    
    LOOP_ROWS: do i=1_ik4,ROWS
      read(21, *) TEMP(:), VECTOR(i)
    end do LOOP_ROWS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_READ(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_READ_COLUMN_INTEGER
  
  
!**********************************************************
!**     Subroutine to read in a certain real-valued      **
!**     column from a .txt-file:                         **
!**********************************************************
  subroutine ROUTINE_READ_COLUMN_REAL(FILE_NAME, ROWS, COLUMN, SKIP_ROWS, VECTOR, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                   :: ROWS, COLUMN, SKIP_ROWS
    integer(ik4)                               :: i, io_error
    real(rkdp), dimension(ROWS), intent(inout) :: VECTOR
    real(rkdp), dimension(COLUMN-1_ik4)        :: TEMP
    logical, intent(in)                        :: SHOW_INFO
    character(len=40)                          :: PLACE_ERROR
    character(len=200), intent(in)             :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_READ_COLUMN_REAL'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SKIP: if (SKIP_ROWS .GT. 0_ik4) then
      LOOP_SKIP: do i=1_ik4,SKIP_ROWS
        read(21, *, iostat=io_error)
        call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
      end do LOOP_SKIP
    end if CHECK_SKIP
    
    LOOP_ROWS: do i=1_ik4,ROWS
      read(21, *) TEMP(:), VECTOR(i)
    end do LOOP_ROWS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_READ(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_READ_COLUMN_REAL
  
  
!**********************************************************
!**     Subroutine to read in a certain row of inte-     **
!**     gers from a .txt-file:                           **
!**********************************************************
  subroutine ROUTINE_READ_ROW_INTEGER(FILE_NAME, ROW, COLUMNS, SKIP_COLUMNS, VECTOR, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                        :: ROW, COLUMNS, SKIP_COLUMNS
    integer(ik4)                                    :: i, io_error
    integer(ik4), dimension(COLUMNS), intent(inout) :: VECTOR
    integer(ik4), dimension(SKIP_COLUMNS)           :: TEMP
    logical, intent(in)                             :: SHOW_INFO
    character(len=40)                               :: PLACE_ERROR
    character(len=200), intent(in)                  :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_READ_ROW_INTEGER'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    LOOP_SKIP: do i=1_ik4,ROW-1_ik4
      read(21, *, iostat=io_error)
      call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_SKIP
    
    CHECK_SKIP: if (SKIP_COLUMNS .GT. 0_ik4) then
      read(21, *) TEMP(1_ik4:SKIP_COLUMNS), VECTOR(1_ik4:COLUMNS)
    else
      read(21, *) VECTOR(1_ik4:COLUMNS)
    end if CHECK_SKIP
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_READ(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_READ_ROW_INTEGER
  
  
!**********************************************************
!**     Subroutine to read in a certain real-valued      **
!**     row from a .txt-file:                            **
!**********************************************************
  subroutine ROUTINE_READ_ROW_REAL(FILE_NAME, ROW, COLUMNS, SKIP_COLUMNS, VECTOR, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)                      :: ROW, COLUMNS, SKIP_COLUMNS
    integer(ik4)                                  :: i, io_error
    real(rkdp), dimension(COLUMNS), intent(inout) :: VECTOR
    real(rkdp), dimension(SKIP_COLUMNS)           :: TEMP
    logical, intent(in)                           :: SHOW_INFO
    character(len=40)                             :: PLACE_ERROR
    character(len=200), intent(in)                :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_READ_ROW_REAL'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    LOOP_SKIP: do i=1_ik4,ROW-1_ik4
      read(21, *, iostat=io_error)
      call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_SKIP
    
    CHECK_SKIP: if (SKIP_COLUMNS .GT. 0_ik4) then
      read(21, *) TEMP(1_ik4:SKIP_COLUMNS), VECTOR(1_ik4:COLUMNS)
    else
      read(21, *) VECTOR(1_ik4:COLUMNS)
    end if CHECK_SKIP
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_READ(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_READ_ROW_REAL
  
  
!**********************************************************
!**     Subroutine to read in a certain value of an      **
!**     integer from a .txt-file:                        **
!**********************************************************
  subroutine ROUTINE_READ_INTEGER(FILE_NAME, ROW, COLUMN, VALUE, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)              :: ROW, COLUMN
    integer(ik4), intent(inout)           :: VALUE
    integer(ik4)                          :: i, io_error
    integer(ik4), dimension(COLUMN-1_ik4) :: TEMP
    logical, intent(in)                   :: SHOW_INFO
    character(len=40)                     :: PLACE_ERROR
    character(len=200), intent(in)        :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_READ_INTEGER'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    LOOP_SKIP: do i=1_ik4,ROW-1_ik4
      read(21, *, iostat=io_error)
      call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_SKIP
    
    read(21, *) TEMP(1_ik4:COLUMN-1_ik4), VALUE
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_READ(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_READ_INTEGER
  
  
!**********************************************************
!**     Subroutine to read in a certain real value       **
!**     from a .txt-file:                                **
!**********************************************************
  subroutine ROUTINE_READ_REAL(FILE_NAME, ROW, COLUMN, VALUE, SHOW_INFO)
    implicit none
    
    integer(ik4), intent(in)              :: ROW, COLUMN
    integer(ik4)                          :: i, io_error
    integer(ik4), dimension(COLUMN-1_ik4) :: TEMP
    real(rkdp), intent(inout)             :: VALUE
    logical, intent(in)                   :: SHOW_INFO
    character(len=40)                     :: PLACE_ERROR
    character(len=200), intent(in)        :: FILE_NAME
    
    PLACE_ERROR = 'ROUTINE_READ_REAL'
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    LOOP_SKIP: do i=1_ik4,ROW-1_ik4
      read(21, *, iostat=io_error)
      call ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_SKIP
    
    read(21, *) TEMP(1_ik4:COLUMN-1_ik4), VALUE
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    CHECK_SHOW_INFO: if (SHOW_INFO) then
      call ROUTINE_INFO_READ(FILE_NAME)
    end if CHECK_SHOW_INFO
  end subroutine ROUTINE_READ_REAL
end module MODULE_LVL3_READ_OPERATIONS
