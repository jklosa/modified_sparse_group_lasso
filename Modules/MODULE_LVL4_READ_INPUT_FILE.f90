!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- MODULE_LVL4_READ_INPUT_FILE.f90 ----        **
!**                                                      **
!**     This module reads all input parameters from      **
!**     the input file. There are subroutines for        **
!**     every box separately:                            **
!**       - ROUTINE_READ_GLOBAL_PARAMETERS               **
!**       - ROUTINE_READ_BOX_PREPROCESSING               **
!**       - ROUTINE_READ_BOX_ANALYZE_DATA                **
!**       - ROUTINE_READ_BOX_CREATE_FIGURES              **
!**                                                      **
!**     This module uses the following modules:          **
!**       - MODULE_LVL1_KIND_NUMBERS                     **
!**       - MODULE_LVL2_ON_SCREEN_MESSAGES               **
!**       - MODULE_LVL3_MEMORY_MANAGEMENT                **
!**                                                      **
!**     LEIBNIZ INSTITUTE (FBN)                          **
!**     Dummerstorf                                      **
!**     Start: 16 August 2016                            **
!**     Author: Jan Klosa                                **
!**                                                      **
!**********************************************************
!**********************************************************

module MODULE_LVL4_READ_INPUT_FILE
  use MODULE_LVL1_KIND_NUMBERS
  use MODULE_LVL2_ON_SCREEN_MESSAGES
  use MODULE_LVL3_MEMORY_MANAGEMENT
  implicit none
  
  contains
  
  
!**********************************************************
!**     Subroutine to determine global parameters:       **
!**********************************************************
  subroutine ROUTINE_READ_GLOBAL_PARAMETERS(FILE_NAME, &
                                          & NUMBER_OF_CHROMOSOMES, &
                                          & NUMBER_OF_EXPERIMENTS, &
                                          & SINGLE_FAMILY_PER_EXPERIMENT, &
                                          & FAMILY_SPECIFIC_ANALYSIS, &
                                          & TYPE_OF_DATA, &
                                          & COVARIANCE_DESIGN, &
                                          & RECODE_HAPLOTYPES, &
                                          & INPUT_PEDIGREE_FILE, &
                                          & OUTPUT_FOLDER_FOR_PREPROCESSING, &
                                          & OUTPUT_FOLDER_FOR_ANALYZE_DATA, &
                                          & PROVIDE_ADDITIONAL_INFORMATION)
    
    implicit none
    
    !********************************************************
    !**     Declare all incoming variables:                **
    !********************************************************
    integer(ik4), intent(inout)       :: NUMBER_OF_CHROMOSOMES, NUMBER_OF_EXPERIMENTS
    logical, intent(inout)            :: SINGLE_FAMILY_PER_EXPERIMENT, FAMILY_SPECIFIC_ANALYSIS
    logical, intent(inout)            :: RECODE_HAPLOTYPES, PROVIDE_ADDITIONAL_INFORMATION
    character(len=200), intent(in)    :: FILE_NAME
    character(len=200), intent(inout) :: TYPE_OF_DATA, COVARIANCE_DESIGN, INPUT_PEDIGREE_FILE, OUTPUT_FOLDER_FOR_PREPROCESSING
    character(len=200), intent(inout) :: OUTPUT_FOLDER_FOR_ANALYZE_DATA
    
    !********************************************************
    !**     Declare all internal variables:                **
    !********************************************************
    integer(ik4)       :: POSITION_INDEX, ROW, LENGTH, i, j, io_error
    logical            :: EMPTY
    character(len=40)  :: PLACE_ERROR
    character(len=200) :: TEMP_STRING, LABEL
    
    !********************************************************
    !**     Default variable initializations:              **
    !********************************************************
    NUMBER_OF_CHROMOSOMES           = 1_ik4
    NUMBER_OF_EXPERIMENTS           = 1_ik4
    SINGLE_FAMILY_PER_EXPERIMENT    = .FALSE.
    FAMILY_SPECIFIC_ANALYSIS        = .FALSE.
    RECODE_HAPLOTYPES               = .FALSE.
    PROVIDE_ADDITIONAL_INFORMATION  = .FALSE.
    TYPE_OF_DATA                    = 'haplotypes'
    COVARIANCE_DESIGN               = 'paternal_and_maternal'
    INPUT_PEDIGREE_FILE             = 'Input/PedigreeAndGeneticValues.txt'
    OUTPUT_FOLDER_FOR_PREPROCESSING = 'Output_PRE'
    OUTPUT_FOLDER_FOR_ANALYZE_DATA  = 'Output_AD'
    
    !********************************************************
    !**     Initialize the remaining variables:            **
    !********************************************************
    POSITION_INDEX = 0_ik4
    ROW            = 0_ik4
    LENGTH         = 0_ik4
    i              = 0_ik4
    j              = 0_ik4
    PLACE_ERROR    = 'ROUTINE_READ_GLOBAL_SETTINGS'
    TEMP_STRING    = ''
    LABEL          = ''
    EMPTY          = .FALSE.
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    !********************************************************
    !**     Loop over all rows of the input file.          **
    !**     Proceed only, if there are no I/O errors:      **
    !********************************************************
    LOOP_ROWS_OF_FILE: do while (io_error .EQ. 0_ik4)
      read(21, '(A)', iostat=io_error) TEMP_STRING
      
      CHECK_IO_STATUS: if (io_error .EQ. 0_ik4) then
        ROW = ROW+1_ik4
        
        POSITION_INDEX = scan(TEMP_STRING, ' ')
        LABEL          = TEMP_STRING(1_ik4:POSITION_INDEX-1_ik4)
        POSITION_INDEX = scan(TEMP_STRING, ':')
        TEMP_STRING    = TEMP_STRING(POSITION_INDEX+1_ik4:)
        TEMP_STRING    = adjustl(TEMP_STRING)
        LENGTH         = len_trim(TEMP_STRING)
        
        !******************************************************
        !**     If the string is not empty, proceed with     **
        !**     "select case" and determine values:          **
        !******************************************************
        CHECK_EMPTY: if (LENGTH .GT. 0_ik4) then
          CASE_LABEL: select case (trim(LABEL))
            case ('NUMBER_OF_CHROMOSOMES')
              read(TEMP_STRING, *) NUMBER_OF_CHROMOSOMES
            
            case ('NUMBER_OF_EXPERIMENTS')
              read(TEMP_STRING, *) NUMBER_OF_EXPERIMENTS
            
            case ('FAMILIES_PER_EXPERIMENT')
              CHECK_ONE_ALL: if ((trim(TEMP_STRING) .EQ. 'One') .OR. (trim(TEMP_STRING) .EQ. 'one')) then
                SINGLE_FAMILY_PER_EXPERIMENT  = .TRUE.
              end if CHECK_ONE_ALL
            
            case ('FAMILY_SPECIFIC_ANALYSIS')
              CHECK_YES_NO_1: if ((trim(TEMP_STRING) .EQ. 'Yes') .OR. (trim(TEMP_STRING) .EQ. 'yes')) then
                FAMILY_SPECIFIC_ANALYSIS = .TRUE.
              end if CHECK_YES_NO_1
            
            case ('TYPE_OF_DATA')
              read(TEMP_STRING, *) TYPE_OF_DATA
            
            case ('COVARIANCE_DESIGN')
              read(TEMP_STRING, *) COVARIANCE_DESIGN
            
            case ('RECODE_HAPLOTYPES')
              CHECK_YES_NO_2: if ((trim(TEMP_STRING) .EQ. 'Yes') .OR. (trim(TEMP_STRING) .EQ. 'yes')) then
                RECODE_HAPLOTYPES = .TRUE.
              end if CHECK_YES_NO_2
            
            case ('INPUT_PEDIGREE_FILE')
              read(TEMP_STRING, *) INPUT_PEDIGREE_FILE
            
            case ('OUTPUT_FOLDER_FOR_PREPROCESSING')
              read(TEMP_STRING, *) OUTPUT_FOLDER_FOR_PREPROCESSING
            
            case ('OUTPUT_FOLDER_FOR_ANALYZE_DATA')
              read(TEMP_STRING, *) OUTPUT_FOLDER_FOR_ANALYZE_DATA
            
            case ('PROVIDE_ADDITIONAL_INFORMATION')
              CHECK_YES_NO_3: if ((trim(TEMP_STRING) .EQ. 'Yes') .OR. (trim(TEMP_STRING) .EQ. 'yes')) then
                PROVIDE_ADDITIONAL_INFORMATION = .TRUE.
              end if CHECK_YES_NO_3
            
          end select CASE_LABEL
        end if CHECK_EMPTY
      end if CHECK_IO_STATUS
    end do LOOP_ROWS_OF_FILE
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    call ROUTINE_PRINT_INPUT_GLOBAL_PARAMETERS(NUMBER_OF_CHROMOSOMES, &
      &  NUMBER_OF_EXPERIMENTS, &
      &  SINGLE_FAMILY_PER_EXPERIMENT, &
      &  FAMILY_SPECIFIC_ANALYSIS, &
      &  TYPE_OF_DATA, &
      &  COVARIANCE_DESIGN, &
      &  RECODE_HAPLOTYPES, &
      &  INPUT_PEDIGREE_FILE, &
      &  OUTPUT_FOLDER_FOR_PREPROCESSING, &
      &  OUTPUT_FOLDER_FOR_ANALYZE_DATA, &
      &  PROVIDE_ADDITIONAL_INFORMATION)
  end subroutine ROUTINE_READ_GLOBAL_PARAMETERS
  
  
!**********************************************************
!**     Subroutine to determine the input parameters     **
!**     for PREPROCESSING:                               **
!**********************************************************
  subroutine ROUTINE_READ_BOX_PREPROCESSING(FILE_NAME, &
                                          & PERFORM_PREPROCESSING, &
                                          & EXCLUDE_FULL_SIBS, &
                                          & WHICH_MODEL_IN_SIMULATION, &
                                          & FAMILY_SIZE_STYLE, &
                                          & NUMBER_KEEP_PERCENT_OFFSPRING, &
                                          & INPUT_SNP_HAPLOTYPE_FILE, &
                                          & INPUT_SNP_INFO_FILE, &
                                          & MAF_THRESHOLD)
    
    implicit none
    
    !********************************************************
    !**     Declare all incoming variables:                **
    !********************************************************
    integer(ik4), intent(inout)       :: WHICH_MODEL_IN_SIMULATION, NUMBER_KEEP_PERCENT_OFFSPRING
    real(rkdp), intent(inout)         :: MAF_THRESHOLD
    logical, intent(inout)            :: PERFORM_PREPROCESSING
    logical, intent(inout)            :: EXCLUDE_FULL_SIBS
    character(len=200), intent(in)    :: FILE_NAME
    character(len=200), intent(inout) :: FAMILY_SIZE_STYLE, INPUT_SNP_HAPLOTYPE_FILE, INPUT_SNP_INFO_FILE
    
    !********************************************************
    !**     Declare all internal variables:                **
    !********************************************************
    integer(ik4)       :: POSITION_INDEX, ROW, LENGTH, i, j, io_error
    logical            :: EMPTY
    character(len=40)  :: PLACE_ERROR
    character(len=200) :: TEMP_STRING, LABEL
    
    !********************************************************
    !**     Default variable initializations:              **
    !********************************************************
    WHICH_MODEL_IN_SIMULATION     = 4_ik4
    NUMBER_KEEP_PERCENT_OFFSPRING = 100_ik4
    PERFORM_PREPROCESSING         = .FALSE.
    EXCLUDE_FULL_SIBS             = .FALSE.
    FAMILY_SIZE_STYLE             = 'balanced'
    INPUT_SNP_HAPLOTYPE_FILE      = 'Input/Chip1Phase.txt'
    INPUT_SNP_INFO_FILE           = 'Input/Chip1SnpInformation.txt'
    MAF_THRESHOLD                 = 0.05_rkdp
    
    !********************************************************
    !**     Initialize the remaining variables:            **
    !********************************************************
    POSITION_INDEX  = 0_ik4
    ROW             = 0_ik4
    LENGTH          = 0_ik4
    i               = 0_ik4
    j               = 0_ik4
    PLACE_ERROR     = 'ROUTINE_READ_BOX_PREPROCESSING'
    TEMP_STRING     = ''
    LABEL           = ''
    EMPTY           = .FALSE.
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    !********************************************************
    !**     Loop over all rows of the input file.          **
    !**     Proceed only, if there are no I/O errors:      **
    !********************************************************
    LOOP_ROWS_OF_FILE: do while (io_error .EQ. 0_ik4)
      read(21, '(A)', iostat=io_error) TEMP_STRING
      
      CHECK_IO_STATUS: if (io_error .EQ. 0_ik4) then
        ROW = ROW+1_ik4
        
        POSITION_INDEX = scan(TEMP_STRING, ' ')
        LABEL          = TEMP_STRING(1_ik4:POSITION_INDEX-1_ik4)
        POSITION_INDEX = scan(TEMP_STRING, ':')
        TEMP_STRING    = TEMP_STRING(POSITION_INDEX+1_ik4:)
        TEMP_STRING    = adjustl(TEMP_STRING)
        LENGTH         = len_trim(TEMP_STRING)
        
        !******************************************************
        !**     If the string is not empty, proceed with     **
        !**     "select case" and determine values:          **
        !******************************************************
        CHECK_EMPTY: if (LENGTH .GT. 0_ik4) then
          CASE_LABEL: select case (trim(LABEL))
            case ('PERFORM_PREPROCESSING')
              CHECK_YES_NO_1: if ((trim(TEMP_STRING) .EQ. 'Yes') .OR. (trim(TEMP_STRING) .EQ. 'yes')) then
                PERFORM_PREPROCESSING = .TRUE.
              end if CHECK_YES_NO_1
            
            case ('EXCLUDE_FULL_SIBS')
              CHECK_YES_NO_2: if ((trim(TEMP_STRING) .EQ. 'Yes') .OR. (trim(TEMP_STRING) .EQ. 'yes')) then
                EXCLUDE_FULL_SIBS = .TRUE.
              end if CHECK_YES_NO_2
            
            case ('WHICH_MODEL_IN_SIMULATION')
              read(TEMP_STRING, *) WHICH_MODEL_IN_SIMULATION
            
            case ('FAMILY_SIZE_STYLE')
              read(TEMP_STRING, *) FAMILY_SIZE_STYLE
            
            case ('NUMBER_KEEP_PERCENT_OFFSPRING')
              read(TEMP_STRING, *) NUMBER_KEEP_PERCENT_OFFSPRING
            
            case ('INPUT_SNP_HAPLOTYPE_FILE')
              read(TEMP_STRING, *) INPUT_SNP_HAPLOTYPE_FILE
            
            case ('INPUT_SNP_INFO_FILE ')
              read(TEMP_STRING, *) INPUT_SNP_INFO_FILE
            
            case ('MINOR_ALLELE_FREQUENCY_THRESHOLD')
              read(TEMP_STRING, *) MAF_THRESHOLD
            
          end select CASE_LABEL
        end if CHECK_EMPTY
      end if CHECK_IO_STATUS
    end do LOOP_ROWS_OF_FILE
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    call ROUTINE_PRINT_INPUT_BOX_PREPROCESSING(PERFORM_PREPROCESSING, &
      &  EXCLUDE_FULL_SIBS, &
      &  WHICH_MODEL_IN_SIMULATION, &
      &  FAMILY_SIZE_STYLE, &
      &  NUMBER_KEEP_PERCENT_OFFSPRING, &
      &  INPUT_SNP_HAPLOTYPE_FILE, &
      &  INPUT_SNP_INFO_FILE, &
      &  MAF_THRESHOLD)
  end subroutine ROUTINE_READ_BOX_PREPROCESSING
  
  
!**********************************************************
!**     Subroutine to determine the input parameters     **
!**     for ANALYZE_DATA:                                **
!**********************************************************
  subroutine ROUTINE_READ_BOX_ANALYZE_DATA(FILE_NAME, &
                                         & PERFORM_ANALYZE_DATA, &
                                         & NUMBER_OF_EXPERIMENTS, &
                                         & DUMMY_SIZE, &
                                         & VECTOR_WHICH_TRAITS, &
                                         & VECTOR_WHICH_EXPERIMENTS, &
                                         & NUMBER_OF_ITERATIONS, &
                                         & INVERSE_COVARIANCE_DESIGN, &
                                         & PATH_TO_SPAI_EXECUTABLE, &
                                         & SOURCE_CODE_DIRECTORY, &
                                         & NUMBER_OF_OFF_DIAGONALS_INVERSE, &
                                         & STYLE_OF_SIGNIFICANCE_TEST, &
                                         & WINDOW_WIDTH_FOR_SIGNIFICANCE, &
                                         & THRESHOLD_FOR_SIGNIFICANCE, &
                                         & OBSERVATIONS_FILE, &
                                         & WHICH_METHOD)
    
    implicit none
    
    !********************************************************
    !**     Declare all incoming variables:                **
    !********************************************************
    integer(ik4), intent(in)                                 :: NUMBER_OF_EXPERIMENTS
    integer(ik4), intent(inout)                              :: DUMMY_SIZE, NUMBER_OF_ITERATIONS, WINDOW_WIDTH_FOR_SIGNIFICANCE
    integer(ik4), intent(inout)                              :: WHICH_METHOD, NUMBER_OF_OFF_DIAGONALS_INVERSE
    integer(ik4), dimension(DUMMY_SIZE), intent(inout)       :: VECTOR_WHICH_TRAITS
    real(rkdp), intent(inout)                                :: THRESHOLD_FOR_SIGNIFICANCE
    logical, intent(inout)                                   :: PERFORM_ANALYZE_DATA
    logical, dimension(NUMBER_OF_EXPERIMENTS), intent(inout) :: VECTOR_WHICH_EXPERIMENTS
    character(len=200), intent(in)                           :: FILE_NAME
    character(len=200), intent(inout)                        :: INVERSE_COVARIANCE_DESIGN, STYLE_OF_SIGNIFICANCE_TEST
    character(len=200), intent(inout)                        :: OBSERVATIONS_FILE, PATH_TO_SPAI_EXECUTABLE, SOURCE_CODE_DIRECTORY
    
    !********************************************************
    !**     Declare all internal variables:                **
    !********************************************************
    integer(ik4)                            :: POSITION_INDEX, ROW, COLUMNS, LENGTH, i, j, io_error
    integer(ik4), allocatable, dimension(:) :: VECTOR_TEMP
    logical                                 :: DELIMITER, EMPTY
    character(len=40)                       :: PLACE_ERROR
    character(len=200)                      :: TEMP_STRING, LABEL
    
    !********************************************************
    !**     Default variable initializations:              **
    !********************************************************
    NUMBER_OF_ITERATIONS             = 50000_ik4
    WINDOW_WIDTH_FOR_SIGNIFICANCE    = 5_ik4
    NUMBER_OF_OFF_DIAGONALS_INVERSE = WINDOW_WIDTH_FOR_SIGNIFICANCE
    WHICH_METHOD                     = 1_ik4
    THRESHOLD_FOR_SIGNIFICANCE       = 0.02_rkdp
    PERFORM_ANALYZE_DATA             = .FALSE.
    INVERSE_COVARIANCE_DESIGN        = 'full'
    PATH_TO_SPAI_EXECUTABLE          = 'spai-3.2/bin/'
    SOURCE_CODE_DIRECTORY            = 'Analyze_Data/'
    STYLE_OF_SIGNIFICANCE_TEST       = 'fixed'
    OBSERVATIONS_FILE                = 'TPV_Model4.txt'
    
    !********************************************************
    !**     Initialize the remaining variables:            **
    !********************************************************
    POSITION_INDEX  = 0_ik4
    ROW             = 0_ik4
    COLUMNS         = 0_ik4
    LENGTH          = 0_ik4
    i               = 0_ik4
    j               = 0_ik4
    PLACE_ERROR     = 'ROUTINE_READ_BOX_PREPROCESSING'
    TEMP_STRING     = ''
    LABEL           = ''
    DELIMITER       = .FALSE.
    EMPTY           = .FALSE.
    
    open(21, file=FILE_NAME, status='old', action='read', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    !********************************************************
    !**     Loop over all rows of the input file.          **
    !**     Proceed only, if there are no I/O errors:      **
    !********************************************************
    LOOP_ROWS_OF_FILE: do while (io_error .EQ. 0_ik4)
      read(21, '(A)', iostat=io_error) TEMP_STRING
      
      CHECK_IO_STATUS: if (io_error .EQ. 0_ik4) then
        ROW = ROW+1_ik4
        
        POSITION_INDEX = scan(TEMP_STRING, ' ')
        LABEL          = TEMP_STRING(1_ik4:POSITION_INDEX-1_ik4)
        POSITION_INDEX = scan(TEMP_STRING, ':')
        TEMP_STRING    = TEMP_STRING(POSITION_INDEX+1_ik4:)
        TEMP_STRING    = adjustl(TEMP_STRING)
        LENGTH         = len_trim(TEMP_STRING)
        
        !******************************************************
        !**     If the string is not empty, proceed with     **
        !**     "select case" and determine values:          **
        !******************************************************
        CHECK_EMPTY: if (LENGTH .GT. 0_ik4) then
          CASE_LABEL: select case (trim(LABEL))
            case ('PERFORM_ANALYZE_DATA')
              CHECK_YES_NO_1: if ((trim(TEMP_STRING) .EQ. 'Yes') .OR. (trim(TEMP_STRING) .EQ. 'yes')) then
                PERFORM_ANALYZE_DATA = .TRUE.
              end if CHECK_YES_NO_1
            
            case ('TRAITS_TO_BE_ANALYZED')
              COLUMNS = 1_ik4
              LOOP_ENTRIES_1: do i=1_ik4,LENGTH
                CHECK_ENTRY_1: if ((TEMP_STRING(i:i) .EQ. ' ') .OR. (TEMP_STRING(i:i) .EQ. ',')) then
                  DELIMITER = .TRUE.
                else
                  CHECK_DELIMITER_1: if (DELIMITER) then
                    COLUMNS = COLUMNS+1_ik4
                  end if CHECK_DELIMITER_1
                  DELIMITER = .FALSE.
                end if CHECK_ENTRY_1
              end do LOOP_ENTRIES_1
              DUMMY_SIZE = COLUMNS
              read(TEMP_STRING, *) VECTOR_WHICH_TRAITS(1_ik4:DUMMY_SIZE)
              
            case ('EXPERIMENTS_TO_BE_ANALYZED')
              COLUMNS = 1_ik4
              LOOP_ENTRIES_2: do i=1_ik4,LENGTH
              CHECK_ENTRY_2: if ((TEMP_STRING(i:i) .EQ. ' ') .OR. (TEMP_STRING(i:i) .EQ. ',')) then
                  DELIMITER = .TRUE.
                else
                  CHECK_DELIMITER_2: if (DELIMITER) then
                    COLUMNS = COLUMNS+1_ik4
                  end if CHECK_DELIMITER_2
                  DELIMITER = .FALSE.
                end if CHECK_ENTRY_2
              end do LOOP_ENTRIES_2
              call ROUTINE_ALLOCATE_VECTOR_INTEGER(COLUMNS, VECTOR_TEMP)
              read(TEMP_STRING, *) VECTOR_TEMP(1_ik4:COLUMNS)
              
              VECTOR_WHICH_EXPERIMENTS = .FALSE.
              LOOP_ENTRIES_TEMP: do i=1_ik4,COLUMNS
                VECTOR_WHICH_EXPERIMENTS(VECTOR_TEMP(i)) = .TRUE.
              end do LOOP_ENTRIES_TEMP
              call ROUTINE_DEALLOCATE_VECTOR_INTEGER(VECTOR_TEMP)
              
            case ('NUMBER_OF_ITERATIONS')
              read(TEMP_STRING, *) NUMBER_OF_ITERATIONS
              
            case ('INVERSE_COVARIANCE_DESIGN')
              read(TEMP_STRING, *) INVERSE_COVARIANCE_DESIGN
            
            case ('PATH_TO_SPAI_EXECUTABLE')
              read(TEMP_STRING, *) PATH_TO_SPAI_EXECUTABLE
            
            case ('SOURCE_CODE_DIRECTORY')
              read(TEMP_STRING, *) SOURCE_CODE_DIRECTORY
            
            case ('NUMBER_OF_OFF_DIAGONALS_INVERSE')
              read(TEMP_STRING, *) NUMBER_OF_OFF_DIAGONALS_INVERSE
            
            case ('STYLE_OF_SIGNIFICANCE_TEST')
              read(TEMP_STRING, *) STYLE_OF_SIGNIFICANCE_TEST
            
            case ('WINDOW_WIDTH_FOR_SIGNIFICANCE')
              read(TEMP_STRING, *) WINDOW_WIDTH_FOR_SIGNIFICANCE
            
            case ('THRESHOLD_FOR_SIGNIFICANCE')
              read(TEMP_STRING, *) THRESHOLD_FOR_SIGNIFICANCE
              
            case ('OBSERVATIONS_FILE')
              read(TEMP_STRING, *) OBSERVATIONS_FILE
              
            case ('WHICH_METHOD')
              read(TEMP_STRING, *) WHICH_METHOD
              
          end select CASE_LABEL
        end if CHECK_EMPTY
      end if CHECK_IO_STATUS
    end do LOOP_ROWS_OF_FILE
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
    
    call ROUTINE_PRINT_INPUT_BOX_ANALYZE_DATA(PERFORM_ANALYZE_DATA, &
      &  NUMBER_OF_EXPERIMENTS, &
      &  DUMMY_SIZE, &
      &  VECTOR_WHICH_TRAITS(1_ik4:DUMMY_SIZE), &
      &  VECTOR_WHICH_EXPERIMENTS, &
      &  NUMBER_OF_ITERATIONS, &
      &  INVERSE_COVARIANCE_DESIGN, &
      &  PATH_TO_SPAI_EXECUTABLE, &
      &  SOURCE_CODE_DIRECTORY, &
      &  NUMBER_OF_OFF_DIAGONALS_INVERSE, &
      &  STYLE_OF_SIGNIFICANCE_TEST, &
      &  WINDOW_WIDTH_FOR_SIGNIFICANCE, &
      &  THRESHOLD_FOR_SIGNIFICANCE, &
      &  OBSERVATIONS_FILE, &
      &  WHICH_METHOD)
  end subroutine ROUTINE_READ_BOX_ANALYZE_DATA
end module MODULE_LVL4_READ_INPUT_FILE
