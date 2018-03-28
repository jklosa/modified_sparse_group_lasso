!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- LASSO.f90 ----                              **
!**                                                      **
!**     LEIBNIZ INSTITUTE (FBN)                          **
!**     Dummerstorf                                      **
!**     Start: 16 March 2018                             **
!**     Author: Jan Klosa                                **
!**                                                      **
!**********************************************************
!**********************************************************

program LASSO
  use MODULE_LVL1_KIND_NUMBERS
  use MODULE_LVL2_CREATE_FORMATTINGS
  use MODULE_LVL2_ON_SCREEN_MESSAGES
  use MODULE_LVL3_MEMORY_MANAGEMENT
  use MODULE_LVL3_READ_OPERATIONS
  use MODULE_LVL3_WRITE_OPERATIONS
  use MODULE_LVL4_MATRIX_OPERATIONS
  use MODULE_LVL5_FUNCTIONS
  use MODULE_TOP_LASSO
  use MODULE_TOP_GROUP_LASSO
  use MODULE_TOP_SPARSE_GROUP_LASSO
  use MODULE_TOP_MODIFIED_SG_LASSO
  implicit none
  
  
  !********************************************************
  !**     Declare all variables:                         **
  !********************************************************
  integer(ik4)                            :: n, p, SKIP_ROWS, ITERATION_MAX, NUMBER_INTERVALS, j
  integer(ik4)                            :: k, NUMBER_GROUPS, NUMBER_INTERVALS_EPSILON !*<--------------------------- NEW FOR GROUP LASSO
  integer(ik4), allocatable, dimension(:) :: VECTOR_GROUP_SIZES, VECTOR_INDEX_START, VECTOR_INDEX_END !*<--------------------------- NEW FOR GROUP LASSO
  real(rkdp)                              :: LAMBDA_MAX, ALPHA, GAMMA, EPSILON_RELATIVE, PROPORTION_XI
  real(rkdp)                              :: EPSILON_MAX, EPSILON_MIN
  real(rkdp), allocatable, dimension(:)   :: VECTOR_Y_SIM, VECTOR_OMEGA_FEATURE
  real(rkdp), allocatable, dimension(:)   :: VECTOR_X_TRANSP_Y, VECTOR_BETA
  real(rkdp), allocatable, dimension(:)   :: VECTOR_OMEGA_GROUP !*<--------------------------- NEW FOR GROUP LASSO
  real(rkdp), allocatable, dimension(:,:) :: MATRIX_X_SIM, MATRIX_X_TRANSP_X
  logical                                 :: SHOW_INFO
  character(len=40)                       :: NAME_VARIABLE, PLACE_ERROR
  character(len=200)                      :: INPUT_FILE_NAME_VECTOR_Y_SIM
  character(len=200)                      :: INPUT_FILE_NAME_MATRIX_X_SIM
  character(len=200)                      :: OUTPUT_FILE_NAME_BETA
  character(len=200)                      :: OUTPUT_FILE_NAME_ITERATIONS
  character(len=200)                      :: OUTPUT_FILE_NAME_LAMBDA_MAX
  character(len=200)                      :: NAME_OF_PROGRAM
  
  
  !********************************************************
  !**     Initialize variables:                          **
  !********************************************************
  n                               = 0_ik4
  p                               = 0_ik4
  SKIP_ROWS                       = 0_ik4
  ITERATION_MAX                   = 1000_ik4
  NUMBER_INTERVALS                = 20_ik4
  j                               = 0_ik4
  k                               = 0_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  NUMBER_GROUPS                   = 3_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  NUMBER_INTERVALS_EPSILON        = 5_ik4
  LAMBDA_MAX                      = 0.0_rkdp
  ALPHA                           = 0.95_rkdp
  GAMMA                           = 0.6_rkdp
  EPSILON_RELATIVE                = 0.0001_rkdp
  PROPORTION_XI                   = 0.1_rkdp
  EPSILON_MAX                     = 0.01_rkdp
  EPSILON_MIN                     = 0.000001_rkdp
  SHOW_INFO                       = .FALSE.
  NAME_VARIABLE                   = ''
  PLACE_ERROR                     = ''
  INPUT_FILE_NAME_VECTOR_Y_SIM    = 'Input/y_sim.txt'
  INPUT_FILE_NAME_MATRIX_X_SIM    = 'Input/X_sim.txt'
  OUTPUT_FILE_NAME_BETA           = 'Output/modified_sg_lasso_output_f90_beta.txt'
  OUTPUT_FILE_NAME_ITERATIONS     = 'Output/modified_sg_lasso_output_f90_iter.txt'
  OUTPUT_FILE_NAME_LAMBDA_MAX     = 'Output/modified_sg_lasso_output_f90_lambda.txt'
  NAME_OF_PROGRAM                 = 'LASSO'
  
  call ROUTINE_INFO_BEGINNING_OF_PROGRAM(NAME_OF_PROGRAM)
  call ROUTINE_DETERMINE_DIMENSION(INPUT_FILE_NAME_MATRIX_X_SIM, n, p, SKIP_ROWS, SHOW_INFO)
  NAME_VARIABLE = 'n'
  call ROUTINE_INFO_VALUE_INTEGER(NAME_VARIABLE, n)
  NAME_VARIABLE = 'p'
  call ROUTINE_INFO_VALUE_INTEGER(NAME_VARIABLE, p)
  
  
  !********************************************************
  !**     Allocate memory:                               **
  !********************************************************
  call ROUTINE_ALLOCATE_VECTOR_INTEGER(NUMBER_GROUPS, VECTOR_GROUP_SIZES) !*<--------------------------- NEW FOR GROUP LASSO
  call ROUTINE_ALLOCATE_VECTOR_INTEGER(NUMBER_GROUPS, VECTOR_INDEX_START) !*<--------------------------- NEW FOR GROUP LASSO
  call ROUTINE_ALLOCATE_VECTOR_INTEGER(NUMBER_GROUPS, VECTOR_INDEX_END) !*<--------------------------- NEW FOR GROUP LASSO
  call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_GROUPS, VECTOR_OMEGA_GROUP) !*<--------------------------- NEW FOR GROUP LASSO
  call ROUTINE_ALLOCATE_VECTOR_REAL(n, VECTOR_Y_SIM)
  call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_BETA)
  call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_OMEGA_FEATURE)
  call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_X_TRANSP_Y)
  call ROUTINE_ALLOCATE_MATRIX_REAL(n, p, MATRIX_X_SIM)
  call ROUTINE_ALLOCATE_MATRIX_REAL(p, p, MATRIX_X_TRANSP_X)
  
  
  !********************************************************
  !**     Read data:                                     **
  !********************************************************
  call ROUTINE_READ_COLUMN_REAL(INPUT_FILE_NAME_VECTOR_Y_SIM, n, 1_ik4, SKIP_ROWS, VECTOR_Y_SIM, SHOW_INFO)
  call ROUTINE_READ_MATRIX_REAL(INPUT_FILE_NAME_MATRIX_X_SIM, n, p, SKIP_ROWS, MATRIX_X_SIM, SHOW_INFO)
  
  
  !********************************************************
  !**     One-time calculations:                         **
  !********************************************************
  call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, p, MATRIX_X_SIM, VECTOR_Y_SIM, VECTOR_X_TRANSP_Y)
  call ROUTINE_MATRIX_TRANSP_MATRIX_MULT_DGEMM(p, n, p, MATRIX_X_SIM, MATRIX_X_SIM, MATRIX_X_TRANSP_X)
  
  
  !******************************************************** !*<--------------------------- NEW FOR GROUP LASSO
  !**     Treatment of groups and rearrangements of      ** !*<--------------------------- NEW FOR GROUP LASSO
  !**     the data:                                      ** !*<--------------------------- NEW FOR GROUP LASSO
  !******************************************************** !*<--------------------------- NEW FOR GROUP LASSO
  VECTOR_GROUP_SIZES(1_ik4) = 3_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  VECTOR_GROUP_SIZES(2_ik4) = 4_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  VECTOR_GROUP_SIZES(3_ik4) = 3_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  
  VECTOR_INDEX_START(1_ik4) = 1_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  VECTOR_INDEX_START(2_ik4) = 4_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  VECTOR_INDEX_START(3_ik4) = 8_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  
  VECTOR_INDEX_END(1_ik4)   = 3_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  VECTOR_INDEX_END(2_ik4)   = 7_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  VECTOR_INDEX_END(3_ik4)   = 10_ik4 !*<--------------------------- NEW FOR GROUP LASSO
  
  
  !********************************************************
  !**     Treatment of weights:                          **
  !********************************************************
  VECTOR_OMEGA_GROUP(1_ik4) = 0.0_rkdp !*<--------------------------- NEW FOR GROUP LASSO
  VECTOR_OMEGA_GROUP(2_ik4) = 1.0_rkdp !*<--------------------------- NEW FOR GROUP LASSO
  VECTOR_OMEGA_GROUP(3_ik4) = 1.0_rkdp !*<--------------------------- NEW FOR GROUP LASSO
  
  LOOP_INIT_WEIGHTS_FEATURE: do k=1_ik4,NUMBER_GROUPS !*<--------------------------- NEW FOR GROUP LASSO
    LOOP_SET_TO_GROUP_WEIGHT: do j=VECTOR_INDEX_START(k),VECTOR_INDEX_END(k) !*<--------------------------- NEW FOR GROUP LASSO
      VECTOR_OMEGA_FEATURE(j) = VECTOR_OMEGA_GROUP(k) !*<--------------------------- NEW FOR GROUP LASSO
    end do LOOP_SET_TO_GROUP_WEIGHT !*<--------------------------- NEW FOR GROUP LASSO
  end do LOOP_INIT_WEIGHTS_FEATURE !*<--------------------------- NEW FOR GROUP LASSO
  
  
  !********************************************************
  !**     Proximal gradient descent with backtracking    **
  !**     line search and warm starts:                   **
  !********************************************************
!  call ROUTINE_LAMBDA_MAX_LASSO(n, p, LAMBDA_MAX, VECTOR_Y_SIM, VECTOR_OMEGA_FEATURE, VECTOR_BETA, MATRIX_X_SIM) !*<--------------------------- NEW FOR GROUP LASSO
!  call ROUTINE_LAMBDA_MAX_GROUP_LASSO(n, p, NUMBER_GROUPS, LAMBDA_MAX, VECTOR_GROUP_SIZES, VECTOR_INDEX_START, &
!       & VECTOR_INDEX_END, VECTOR_Y_SIM, VECTOR_OMEGA_GROUP, VECTOR_OMEGA_FEATURE, VECTOR_BETA, MATRIX_X_SIM) !*<--------------------------- NEW FOR GROUP LASSO
!  call ROUTINE_LAMBDA_MAX_SPARSE_GROUP_LASSO(n, p, NUMBER_GROUPS, ALPHA, LAMBDA_MAX, VECTOR_GROUP_SIZES, &
!       & VECTOR_INDEX_START, VECTOR_INDEX_END, VECTOR_Y_SIM, VECTOR_OMEGA_GROUP, VECTOR_OMEGA_FEATURE, &
!       & VECTOR_BETA, MATRIX_X_SIM)
  call ROUTINE_LAMBDA_MAX_MODIFIED_SG_LASSO(n, p, NUMBER_GROUPS, ALPHA, LAMBDA_MAX, VECTOR_GROUP_SIZES, &
       & VECTOR_INDEX_START, VECTOR_INDEX_END, VECTOR_Y_SIM, VECTOR_OMEGA_GROUP, VECTOR_OMEGA_FEATURE, &
       & VECTOR_BETA, MATRIX_X_SIM)
  NAME_VARIABLE = 'LAMBDA_MAX'
  call ROUTINE_INFO_VALUE_REAL(NAME_VARIABLE, LAMBDA_MAX)
  
!  call ROUTINE_LASSO_PROX_GRAD_BACK_WARM( &
!       & n, &
!       & p, &
!       & ITERATION_MAX, &
!       & NUMBER_INTERVALS, &
!       & LAMBDA_MAX, &
!       & GAMMA, &
!       & EPSILON_RELATIVE, &
!       & PROPORTION_XI, &
!       & VECTOR_Y_SIM, &
!       & VECTOR_OMEGA_FEATURE, &
!       & VECTOR_X_TRANSP_Y, &
!       & VECTOR_BETA, &
!       & MATRIX_X_SIM, &
!       & MATRIX_X_TRANSP_X, &
!       & SHOW_INFO, &
!       & OUTPUT_FILE_NAME_BETA, &
!       & OUTPUT_FILE_NAME_ITERATIONS, &
!       & OUTPUT_FILE_NAME_LAMBDA_MAX)
  
!  call ROUTINE_GROUP_LASSO_PROX_GRAD_BACK_WARM( &
!       & n, &
!       & p, &
!       & NUMBER_GROUPS, &
!       & ITERATION_MAX, &
!       & NUMBER_INTERVALS, &
!       & LAMBDA_MAX, &
!       & GAMMA, &
!       & EPSILON_RELATIVE, &
!       & PROPORTION_XI, &
!       & VECTOR_GROUP_SIZES, &
!       & VECTOR_INDEX_START, &
!       & VECTOR_INDEX_END, &
!       & VECTOR_Y_SIM, &
!       & VECTOR_OMEGA_GROUP, &
!       & VECTOR_OMEGA_FEATURE, &
!       & VECTOR_X_TRANSP_Y, &
!       & VECTOR_BETA, &
!       & MATRIX_X_SIM, &
!       & MATRIX_X_TRANSP_X, &
!       & SHOW_INFO, &
!       & OUTPUT_FILE_NAME_BETA, &
!       & OUTPUT_FILE_NAME_ITERATIONS, &
!       & OUTPUT_FILE_NAME_LAMBDA_MAX)
  
!  call ROUTINE_SPARSE_GROUP_LASSO_PROX_GRAD_BACK_WARM( &
!       & n, &
!       & p, &
!       & NUMBER_GROUPS, &
!       & ITERATION_MAX, &
!       & NUMBER_INTERVALS, &
!       & ALPHA, &
!       & LAMBDA_MAX, &
!       & GAMMA, &
!       & EPSILON_RELATIVE, &
!       & PROPORTION_XI, &
!       & VECTOR_GROUP_SIZES, &
!       & VECTOR_INDEX_START, &
!       & VECTOR_INDEX_END, &
!       & VECTOR_Y_SIM, &
!       & VECTOR_OMEGA_GROUP, &
!       & VECTOR_OMEGA_FEATURE, &
!       & VECTOR_X_TRANSP_Y, &
!       & VECTOR_BETA, &
!       & MATRIX_X_SIM, &
!       & MATRIX_X_TRANSP_X, &
!       & SHOW_INFO, &
!       & OUTPUT_FILE_NAME_BETA, &
!       & OUTPUT_FILE_NAME_ITERATIONS, &
!       & OUTPUT_FILE_NAME_LAMBDA_MAX)
  
  call ROUTINE_MODIFIED_SG_LASSO_PROX_GRAD_BACK_WARM( &
       & n, &
       & p, &
       & NUMBER_GROUPS, &
       & ITERATION_MAX, &
       & NUMBER_INTERVALS, &
       & NUMBER_INTERVALS_EPSILON, &
       & ALPHA, &
       & LAMBDA_MAX, &
       & GAMMA, &
       & EPSILON_RELATIVE, &
       & PROPORTION_XI, &
       & EPSILON_MAX, &
       & EPSILON_MIN, &
       & VECTOR_GROUP_SIZES, &
       & VECTOR_INDEX_START, &
       & VECTOR_INDEX_END, &
       & VECTOR_Y_SIM, &
       & VECTOR_OMEGA_GROUP, &
       & VECTOR_OMEGA_FEATURE, &
       & VECTOR_X_TRANSP_Y, &
       & VECTOR_BETA, &
       & MATRIX_X_SIM, &
       & MATRIX_X_TRANSP_X, &
       & SHOW_INFO, &
       & OUTPUT_FILE_NAME_BETA, &
       & OUTPUT_FILE_NAME_ITERATIONS, &
       & OUTPUT_FILE_NAME_LAMBDA_MAX)
  
  
  !********************************************************
  !**     Deallocate memory:                             **
  !********************************************************
  call ROUTINE_DEALLOCATE_VECTOR_INTEGER(VECTOR_GROUP_SIZES) !*<--------------------------- NEW FOR GROUP LASSO
  call ROUTINE_DEALLOCATE_VECTOR_INTEGER(VECTOR_INDEX_START) !*<--------------------------- NEW FOR GROUP LASSO
  call ROUTINE_DEALLOCATE_VECTOR_INTEGER(VECTOR_INDEX_END) !*<--------------------------- NEW FOR GROUP LASSO
  call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_OMEGA_GROUP) !*<--------------------------- NEW FOR GROUP LASSO
  call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_Y_SIM)
  call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_BETA)
  call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_OMEGA_FEATURE)
  call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_X_TRANSP_Y)
  call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_X_SIM)
  call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_X_TRANSP_X)
  
  
  call ROUTINE_INFO_END_OF_PROGRAM()
end program LASSO
