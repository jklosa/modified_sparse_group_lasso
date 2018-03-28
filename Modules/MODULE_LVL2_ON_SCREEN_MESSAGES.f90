!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- MODULE_LVL2_ON_SCREEN_MESSAGES.f90 ----     **
!**                                                      **
!**     This module contains the following subroutines   **
!**     for "on-screen-response" messages:               **
!**       - ROUTINE_ERROR_MESSAGE_ALLOCATION             **
!**       - ROUTINE_ERROR_MESSAGE_DEALLOCATION           **
!**       - ROUTINE_ERROR_MESSAGE_OPEN                   **
!**       - ROUTINE_ERROR_MESSAGE_CLOSE                  **
!**       - ROUTINE_ERROR_MESSAGE_READ                   **
!**       - ROUTINE_ERROR_MESSAGE_WRITE                  **
!**       - ROUTINE_ERROR_MESSAGE_INFO_LAPACK            **
!**       - ROUTINE_INFO_READ                            **
!**       - ROUTINE_INFO_READ_EMPTY_FILE                 **
!**       - ROUTINE_INFO_WRITE                           **
!**       - ROUTINE_INFO_VALUE_INTEGER                   **
!**       - ROUTINE_INFO_VALUE_REAL                      **
!**       - ROUTINE_INFO_VALUE_STRING                    **
!**       - ROUTINE_INFO_ELAPSED_TIME                    **
!**       - ROUTINE_INFO_BEGINNING_OF_PROGRAM            **
!**       - ROUTINE_INFO_END_OF_PROGRAM                  **
!**       - ROUTINE_PRINT_INTEGER                        **
!**       - ROUTINE_PRINT_REAL                           **
!**       - ROUTINE_PRINT_ITERATION                      **
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

module MODULE_LVL2_ON_SCREEN_MESSAGES
  use MODULE_LVL1_KIND_NUMBERS
  implicit none
  
  contains
  
  
!**********************************************************
!**     Message for non-successful memory allocation:    **
!**********************************************************
  subroutine ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, error)
    implicit none
    
    integer(ik4), intent(in)      :: error
    character(len=40), intent(in) :: NAME_VARIABLE, PLACE_ERROR
    
    if (error .NE. 0_ik4) then
      write(*, *)
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *) '%                         WARNING:                         %'
      write(*, *) '%             Non-successful memory allocation.            %'
      write(*, *) '%                                                          %'
      write(*, *) '%      NAME : ', NAME_VARIABLE                     , '     %'
      write(*, *) '%      PLACE: ', PLACE_ERROR                       , '     %'
      write(*, *) '%      ERROR: ', error  , '                                %'
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *)
      stop
    end if
  end subroutine ROUTINE_ERROR_MESSAGE_ALLOCATION
  
  
!**********************************************************
!**     Message for non-succesful memory deallocation:   **
!**********************************************************
  subroutine ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, error)
    implicit none
    
    integer(ik4), intent(in)      :: error
    character(len=40), intent(in) :: NAME_VARIABLE, PLACE_ERROR
    
    if (error .NE. 0_ik4) then
      write(*, *)
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *) '%                         WARNING:                         %'
      write(*, *) '%            Non-successful memory deallocation.           %'
      write(*, *) '%                                                          %'
      write(*, *) '%      NAME : ', NAME_VARIABLE                     , '     %'
      write(*, *) '%      PLACE: ', PLACE_ERROR                       , '     %'
      write(*, *) '%      ERROR: ', error  , '                                %'
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *)
      stop
    end if
  end subroutine ROUTINE_ERROR_MESSAGE_DEALLOCATION
  
  
!**********************************************************
!**     Message for non-successful file opening:         **
!**********************************************************
  subroutine ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, error)
    implicit none
    
    integer(ik4), intent(in)      :: error
    character(len=40), intent(in) :: FILE_NAME, PLACE_ERROR
    
    if (error .NE. 0_ik4) then
      write(*, *)
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *) '%                         WARNING:                         %'
      write(*, *) '%               Non-successful file opening.               %'
      write(*, *) '%                                                          %'
      write(*, *) '%      NAME : ', FILE_NAME                         , '     %'
      write(*, *) '%      PLACE: ', PLACE_ERROR                       , '     %'
      write(*, *) '%      ERROR: ', error  , '                                %'
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *)
      stop
    end if
  end subroutine ROUTINE_ERROR_MESSAGE_OPEN
  
  
!**********************************************************
!**     Message for non-successful file closing:         **
!**********************************************************
  subroutine ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, error)
    implicit none
    
    integer(ik4), intent(in)      :: error
    character(len=40), intent(in) :: FILE_NAME, PLACE_ERROR
    
    if (error .NE. 0_ik4) then
      write(*, *)
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *) '%                         WARNING:                         %'
      write(*, *) '%               Non-successful file closing.               %'
      write(*, *) '%                                                          %'
      write(*, *) '%      NAME : ', FILE_NAME                         , '     %'
      write(*, *) '%      PLACE: ', PLACE_ERROR                       , '     %'
      write(*, *) '%      ERROR: ', error  , '                                %'
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *)
      stop
    end if
  end subroutine ROUTINE_ERROR_MESSAGE_CLOSE
  
  
!**********************************************************
!**     Message for non-successful file reading:         **
!**********************************************************
  subroutine ROUTINE_ERROR_MESSAGE_READ(FILE_NAME, PLACE_ERROR, error)
    implicit none
    
    integer(ik4), intent(in)      :: error
    character(len=40), intent(in) :: FILE_NAME, PLACE_ERROR
    
    if (error .NE. 0_ik4) then
      write(*, *)
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *) '%                         WARNING:                         %'
      write(*, *) '%               Non-successful file reading.               %'
      write(*, *) '%                                                          %'
      write(*, *) '%      NAME : ', FILE_NAME                         , '     %'
      write(*, *) '%      PLACE: ', PLACE_ERROR                       , '     %'
      write(*, *) '%      ERROR: ', error  , '                                %'
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *)
      stop
    end if
  end subroutine ROUTINE_ERROR_MESSAGE_READ
  
  
!**********************************************************
!**     Message for non-successful file writing:         **
!**********************************************************
  subroutine ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, error)
    implicit none
    
    integer(ik4), intent(in)      :: error
    character(len=40), intent(in) :: FILE_NAME, PLACE_ERROR
    
    if (error .NE. 0_ik4) then
      write(*, *)
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *) '%                         WARNING:                         %'
      write(*, *) '%               Non-successful file writing.               %'
      write(*, *) '%                                                          %'
      write(*, *) '%      NAME : ', FILE_NAME                         , '     %'
      write(*, *) '%      PLACE: ', PLACE_ERROR                       , '     %'
      write(*, *) '%      ERROR: ', error  , '                                %'
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *)
      stop
    end if
  end subroutine ROUTINE_ERROR_MESSAGE_WRITE
  
  
!**********************************************************
!**     Message for non-succesful use of LAPACK rou-     **
!**     tine:                                            **
!**********************************************************
  subroutine ROUTINE_ERROR_MESSAGE_INFO_LAPACK(INFO, NAME_ROUTINE)
    implicit none
    
    integer(ik4), intent(in)      :: INFO
    character(len=40), intent(in) :: NAME_ROUTINE
    character(len=40)             :: CHAR_INFO
    
    CHECK_VALUE: if (INFO .NE. 0_ik4) then
      write(CHAR_INFO, '(I40)') INFO
      CHAR_INFO = adjustl(CHAR_INFO)
      
      write(*, *)
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *) '%                         WARNING:                         %'
      write(*, *) '%          Non-successful use of LAPACK routine.           %'
      write(*, *) '%                                                          %'
      write(*, *) '%      NAME : ', NAME_ROUTINE                      , '     %'
      write(*, *) '%      INFO : ', CHAR_INFO                         , '     %'
      write(*, *) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      write(*, *)
      stop
    end if CHECK_VALUE
  end subroutine ROUTINE_ERROR_MESSAGE_INFO_LAPACK
  
  
!**********************************************************
!**     Message for successful file reading:             **
!**********************************************************
  subroutine ROUTINE_INFO_READ(FILE_NAME)
    implicit none
    
    character(len=40), intent(in) :: FILE_NAME
    
    write(*, *)
    write(*, *) '************************************************************'
    write(*, *) '*                       INFORMATION:                       *'
    write(*, *) '*                File was read successfully.               *'
    write(*, *) '*                                                          *'
    write(*, *) '*      NAME : ', FILE_NAME                         , '     *'
    write(*, *) '************************************************************'
    write(*, *)
  end subroutine ROUTINE_INFO_READ
  
  
!**********************************************************
!**     Message if empty file has been read:             **
!**********************************************************
  subroutine ROUTINE_INFO_READ_EMPTY_FILE(FILE_NAME)
    implicit none
    
    character(len=40), intent(in) :: FILE_NAME
    
    write(*, *)
    write(*, *) '************************************************************'
    write(*, *) '*                       INFORMATION:                       *'
    write(*, *) '*           End of file reached or file is empty.          *'
    write(*, *) '*                                                          *'
    write(*, *) '*      NAME : ', FILE_NAME                         , '     *'
    write(*, *) '************************************************************'
    write(*, *)
  end subroutine ROUTINE_INFO_READ_EMPTY_FILE
  
  
!**********************************************************
!**     Message for successful file writing:             **
!**********************************************************
  subroutine ROUTINE_INFO_WRITE(FILE_NAME)
    implicit none
    
    character(len=40), intent(in) :: FILE_NAME
    
    write(*, *)
    write(*, *) '************************************************************'
    write(*, *) '*                       INFORMATION:                       *'
    write(*, *) '*              File was written successfully.              *'
    write(*, *) '*                                                          *'
    write(*, *) '*      NAME : ', FILE_NAME                         , '     *'
    write(*, *) '************************************************************'
    write(*, *)
  end subroutine ROUTINE_INFO_WRITE
  
  
!**********************************************************
!**     Message to print the value of an integer-        **
!**     variable:                                        **
!**********************************************************
  subroutine ROUTINE_INFO_VALUE_INTEGER(NAME_VARIABLE, VALUE)
    implicit none
    
    integer(ik4), intent(in)      :: VALUE
    character(len=40), intent(in) :: NAME_VARIABLE
    character(len=40)             :: CHAR_VALUE
    
    write(CHAR_VALUE, *) VALUE
    CHAR_VALUE = adjustl(CHAR_VALUE)
    
    write(*, *)
    write(*, *) '************************************************************'
    write(*, *) '*                       INFORMATION:                       *'
    write(*, *) '*            The following value was determined:           *'
    write(*, *) '*                                                          *'
    write(*, *) '*      VARIABLE : ', NAME_VARIABLE                     , ' *'
    write(*, *) '*      VALUE    : ', CHAR_VALUE                        , ' *'
    write(*, *) '************************************************************'
    write(*, *)
  end subroutine ROUTINE_INFO_VALUE_INTEGER
  
  
!**********************************************************
!**     Print a real value of a certain variable in      **
!**     a "standard" format:                             **
!**********************************************************
  subroutine ROUTINE_INFO_VALUE_REAL(NAME_VARIABLE, VALUE)
    implicit none
    
    real(rkdp), intent(in)        :: VALUE
    character(len=40), intent(in) :: NAME_VARIABLE
    character(len=40)             :: CHAR_VALUE
    
    write(CHAR_VALUE, *) VALUE
    CHAR_VALUE = adjustl(CHAR_VALUE)
    
    write(*, *)
    write(*, *) '************************************************************'
    write(*, *) '*                       INFORMATION:                       *'
    write(*, *) '*            The following value was determined:           *'
    write(*, *) '*                                                          *'
    write(*, *) '*      VARIABLE : ', NAME_VARIABLE                     , ' *'
    write(*, *) '*      VALUE    : ', CHAR_VALUE                        , ' *'
    write(*, *) '************************************************************'
    write(*, *)
  end subroutine ROUTINE_INFO_VALUE_REAL
  
  
!**********************************************************
!**     Print a real value of a certain variable in      **
!**     a "standard" format:                             **
!**********************************************************
  subroutine ROUTINE_INFO_VALUE_STRING(NAME_VARIABLE, VALUE)
    implicit none
    
    character(len=40), intent(in) :: NAME_VARIABLE, VALUE
    
    write(*, *)
    write(*, *) '************************************************************'
    write(*, *) '*                       INFORMATION:                       *'
    write(*, *) '*            The following value was determined:           *'
    write(*, *) '*                                                          *'
    write(*, *) '*      VARIABLE : ', NAME_VARIABLE                     , ' *'
    write(*, *) '*      VALUE    : ', VALUE                             , ' *'
    write(*, *) '************************************************************'
    write(*, *)
  end subroutine ROUTINE_INFO_VALUE_STRING
  
  
!**********************************************************
!**     Message to print the elapsed time:               **
!**********************************************************
  subroutine ROUTINE_INFO_ELAPSED_TIME(TIME_1, TIME_2, TIME_RATE)
    implicit none
    
    integer(ik4), intent(in) :: TIME_1, TIME_2, TIME_RATE
    real(rkdp)               :: ELAPSED_TIME_IN_SECONDS
    character(len=30)        :: CHAR_TIME
    
    ELAPSED_TIME_IN_SECONDS = real(TIME_2-TIME_1,rkdp)/real(TIME_RATE,rkdp)
    write(CHAR_TIME, '(E20.8)') ELAPSED_TIME_IN_SECONDS
    CHAR_TIME = adjustl(CHAR_TIME)
    CHAR_TIME = trim(CHAR_TIME)//' seconds'
    
    write(*, *)
    write(*, *) '************************************************************'
    write(*, *) '*                       INFORMATION:                       *'
    write(*, *) '*                                                          *'
    write(*, *) '*      ELAPSED TIME: ', CHAR_TIME               , '        *'
    write(*, *) '************************************************************'
    write(*, *)
  end subroutine ROUTINE_INFO_ELAPSED_TIME
  
  
!**********************************************************
!**     Message to be printed, if the performance of     **
!**     the program begins:                              **
!**********************************************************
  subroutine ROUTINE_INFO_BEGINNING_OF_PROGRAM(NAME_OF_PROGRAM)
    implicit none
    
    integer(ik4)                      :: i = 0_ik4
    character(len=200), intent(inout) :: NAME_OF_PROGRAM
    character(len=30)                 :: CHAR_NEW_NAME = ''
    
    NAME_OF_PROGRAM = trim(adjustl(NAME_OF_PROGRAM))
    write(CHAR_NEW_NAME, '(A30)') NAME_OF_PROGRAM
    
    write(*, *)
    write(*, *) '***********************************************************************'
    write(*, *) '***********************************************************************'
    write(*, *) '**                                                                   **'
    write(*, *) '**                            INFORMATION:                           **'
    write(*, *) '**     Start with performance of: ', CHAR_NEW_NAME            ,'     **'
    write(*, *) '**                                                                   **'
    write(*, *) '***********************************************************************'
    write(*, *) '***********************************************************************'
    write(*, *)
  end subroutine ROUTINE_INFO_BEGINNING_OF_PROGRAM
  
  
!**********************************************************
!**     Message to be printed, if the end of the pro-    **
!**     gram is reached:                                 **
!**********************************************************
  subroutine ROUTINE_INFO_END_OF_PROGRAM()
    implicit none
    
    write(*, *)
    write(*, *) '***********************************************************************'
    write(*, *) '***********************************************************************'
    write(*, *) '**                                                                   **'
    write(*, *) '**                            INFORMATION:                           **'
    write(*, *) '**                 The end of the program is reached!                **'
    write(*, *) '**                                                                   **'
    write(*, *) '***********************************************************************'
    write(*, *) '***********************************************************************'
    write(*, *)
  end subroutine ROUTINE_INFO_END_OF_PROGRAM
  
  
!**********************************************************
!**     Print the value of an integer:                   **
!**********************************************************
  subroutine ROUTINE_PRINT_INTEGER(VALUE)
    implicit none
    
    integer(ik4), intent(in) :: VALUE
    
    write(*, '(1x,A,I10,A)') '*      VALUE : ', VALUE, '                                  *'
  end subroutine ROUTINE_PRINT_INTEGER
  
  
!**********************************************************
!**     Print the value of a real number:                **
!**********************************************************
  subroutine ROUTINE_PRINT_REAL(VALUE)
    implicit none
    
    real(rkdp), intent(in) :: VALUE
    
    write(*, '(1x,A,F24.16,A)') '*      VALUE : ', VALUE           , '                    *'
  end subroutine ROUTINE_PRINT_REAL
  
  
!**********************************************************
!**     Print the number of the current iteration:       **
!**********************************************************
  subroutine ROUTINE_PRINT_ITERATION(VALUE)
    implicit none
    
    integer(ik4), intent(in) :: VALUE
    
    write(*, '(1x,A,I10,A)') '*      ITERATION : ', VALUE , '                              *'
  end subroutine ROUTINE_PRINT_ITERATION
end module MODULE_LVL2_ON_SCREEN_MESSAGES
