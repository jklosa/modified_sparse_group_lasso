!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- MODULE_TOP_GROUP_LASSO.f90 ----             **
!**                                                      **
!**     This module contains the following subroutines   **
!**     to solve the minimization problem:               **
!**       - ROUTINE_GROUP_LASSO_PROX_GRAD_BACK_WARM      **
!**                                                      **
!**     This module uses the following modules:          **
!**       - MODULE_LVL1_KIND_NUMBERS                     **
!**       - MODULE_LVL2_ON_SCREEN_MESSAGES               **
!**       - MODULE_LVL3_MEMORY_MANAGEMENT                **
!**       - MODULE_LVL3_WRITE_OPERATIONS                 **
!**       - MODULE_LVL4_MATRIX_OPERATIONS                **
!**       - MODULE_LVL5_FUNCTIONS                        **
!**                                                      **
!**     LEIBNIZ INSTITUTE (FBN)                          **
!**     Dummerstorf                                      **
!**     Start: 16 March 2018                             **
!**     Author: Jan Klosa                                **
!**                                                      **
!**********************************************************
!**********************************************************

module MODULE_TOP_GROUP_LASSO
  use MODULE_LVL1_KIND_NUMBERS
  use MODULE_LVL2_ON_SCREEN_MESSAGES
  use MODULE_LVL3_MEMORY_MANAGEMENT
  use MODULE_LVL3_WRITE_OPERATIONS
  use MODULE_LVL4_MATRIX_OPERATIONS
  use MODULE_LVL5_FUNCTIONS
  implicit none
  
  contains
  
  
!**********************************************************
!**     Subroutine to solve the minimization problem     **
!**     with the following group lasso algorithm:        **
!**                                                      **
!**       Proximal gradient descent with backtracking    **
!**       line search and warm starts                    **
!**                                                      **
!**********************************************************
  subroutine ROUTINE_GROUP_LASSO_PROX_GRAD_BACK_WARM( &
             & n, &
             & p, &
             & NUMBER_GROUPS, &
             & ITERATION_MAX, &
             & NUMBER_INTERVALS, &
             & LAMBDA_MAX, &
             & GAMMA, &
             & EPSILON_RELATIVE, &
             & PROPORTION_XI, &
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
    
    implicit none
    
    
    !******************************************************
    !**     Declare all incoming variables:              **
    !******************************************************
    integer(ik4), intent(in)                           :: n, p, NUMBER_GROUPS, ITERATION_MAX, NUMBER_INTERVALS
    integer(ik4), dimension(NUMBER_GROUPS), intent(in) :: VECTOR_GROUP_SIZES, VECTOR_INDEX_START, VECTOR_INDEX_END
    real(rkdp), intent(in)                             :: LAMBDA_MAX, GAMMA, EPSILON_RELATIVE, PROPORTION_XI
    real(rkdp), dimension(n), intent(in)               :: VECTOR_Y_SIM
    real(rkdp), dimension(NUMBER_GROUPS), intent(in)   :: VECTOR_OMEGA_GROUP
    real(rkdp), dimension(p), intent(in)               :: VECTOR_OMEGA_FEATURE, VECTOR_X_TRANSP_Y
    real(rkdp), dimension(p), intent(inout)            :: VECTOR_BETA
    real(rkdp), dimension(n,p), intent(in)             :: MATRIX_X_SIM
    real(rkdp), dimension(p,p), intent(in)             :: MATRIX_X_TRANSP_X
    logical, intent(in)                                :: SHOW_INFO
    character(len=200), intent(in)                     :: OUTPUT_FILE_NAME_BETA, OUTPUT_FILE_NAME_ITERATIONS
    character(len=200), intent(in)                     :: OUTPUT_FILE_NAME_LAMBDA_MAX
    
    
    !******************************************************
    !**     Declare new variables:                       **
    !******************************************************
    integer(ik4), save                    :: i, j, k, COUNTER, io_error
    real(rkdp), save                      :: LAMBDA, t, TEMP1, TEMP2, TEMP3
    real(rkdp), allocatable, dimension(:) :: VECTOR_BETA_NEW, VECTOR_GRADIENT, VECTOR_TEMP1, VECTOR_TEMP2
    logical, save                         :: ACCURACY_REACHED, CRITERION_FULFILLED
    character(len=40)                     :: NAME_VARIABLE, PLACE_ERROR
    
    
    !******************************************************
    !**     Initialize variables:                        **
    !******************************************************
    i                   = 0_ik4
    j                   = 0_ik4
    k                   = 0_ik4
    COUNTER             = 0_ik4
    LAMBDA              = 0.0_rkdp
    t                   = 0.0_rkdp
    TEMP1               = 0.0_rkdp
    TEMP2               = 0.0_rkdp
    TEMP3               = 0.0_rkdp
    ACCURACY_REACHED    = .FALSE.
    CRITERION_FULFILLED = .FALSE.
    PLACE_ERROR         = 'ROUTINE_GROUP_LASSO_PROX_GRAD_BACK_WARM'
    
    
    !******************************************************
    !**     Allocate memory:                             **
    !******************************************************
    call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_BETA_NEW)
    call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_GRADIENT)
    
    
    !******************************************************
    !**     The actual solver:                           **
    !******************************************************
    LOOP_LAMBDA: do i=1_ik4,NUMBER_INTERVALS
      ACCURACY_REACHED = .FALSE.
      COUNTER          = 1_ik4
      LAMBDA           = LAMBDA_MAX*exp((real(i-1_ik4,rkdp)/real(NUMBER_INTERVALS-1_ik4,rkdp))*log(PROPORTION_XI))
      
      LOOP_ACCURACY_ITERATION: do while ((.NOT. ACCURACY_REACHED) .AND. (COUNTER .LE. ITERATION_MAX))
        
        !* Calculate t(X)*X*beta
        call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(p, p, MATRIX_X_TRANSP_X, VECTOR_BETA, VECTOR_GRADIENT)
        
        !* Calculate gradient = t(X)*X*beta - t(X)y
        LOOP_GRADIENT: do j=1_ik4,p
          VECTOR_GRADIENT(j) = VECTOR_GRADIENT(j) - VECTOR_X_TRANSP_Y(j)
        end do LOOP_GRADIENT
        CRITERION_FULFILLED = .FALSE.
        t = 1.0_rkdp
        
        LOOP_CRITERION: do while (.NOT. CRITERION_FULFILLED)
          
          !* groupwise soft-scaling to obtain beta_new
          LOOP_GROUPS: do k=1_ik4,NUMBER_GROUPS
            call ROUTINE_ALLOCATE_VECTOR_REAL(VECTOR_GROUP_SIZES(k), VECTOR_TEMP1)
            call ROUTINE_ALLOCATE_VECTOR_REAL(VECTOR_GROUP_SIZES(k), VECTOR_TEMP2)
            
            LOOP_FILL_TEMP_GROUP_1: do j=1,VECTOR_GROUP_SIZES(k)
              VECTOR_TEMP1(j) = VECTOR_BETA(VECTOR_INDEX_START(k)+j-1) - t*VECTOR_GRADIENT(VECTOR_INDEX_START(k)+j-1)
            end do LOOP_FILL_TEMP_GROUP_1
            TEMP1 = t*LAMBDA*VECTOR_OMEGA_GROUP(k)*real(n,rkdp)*sqrt(real(VECTOR_GROUP_SIZES(k),rkdp))
            call ROUTINE_SOFT_SCALING_SCALAR(VECTOR_GROUP_SIZES(k), TEMP1, VECTOR_TEMP1, VECTOR_TEMP2)
            
            LOOP_FILL_TEMP_GROUP_2: do j=1,VECTOR_GROUP_SIZES(k)
              VECTOR_BETA_NEW(VECTOR_INDEX_START(k)+j-1) = VECTOR_TEMP2(j)
            end do LOOP_FILL_TEMP_GROUP_2
            call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_TEMP1)
            call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_TEMP2)
          end do LOOP_GROUPS
          
          call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_TEMP1)
          call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_TEMP2)
          
          !* beta-beta_new
          LOOP_FILL_TEMP_2: do j=1_ik4,p
            VECTOR_TEMP1(j) = VECTOR_BETA(j) - VECTOR_BETA_NEW(j)
          end do LOOP_FILL_TEMP_2
          TEMP1 = 0.0_rkdp
          TEMP2 = 0.0_rkdp
          TEMP3 = 0.0_rkdp
          
          !* loss_function(beta)
          call ROUTINE_HALF_L2_SQUARE_LOSS_FUNCTION(n, p, VECTOR_Y_SIM, VECTOR_BETA, MATRIX_X_SIM, TEMP1)
          
          !* t(grad)*(beta-beta_new) and l2_norm_squared(beta-beta_new)
          LOOP_COLUMNS_1: do j=1_ik4,p
            TEMP2 = TEMP2 + VECTOR_GRADIENT(j)*VECTOR_TEMP1(j)
            TEMP3 = TEMP3 + VECTOR_TEMP1(j)*VECTOR_TEMP1(j)
          end do LOOP_COLUMNS_1
          TEMP1 = TEMP1 - TEMP2 + 0.5_rkdp*TEMP3/t
          
          !* loss_function(beta_new)
          call ROUTINE_HALF_L2_SQUARE_LOSS_FUNCTION(n, p, VECTOR_Y_SIM, VECTOR_BETA_NEW, MATRIX_X_SIM, TEMP2)
          
          CHECK_BACKTRACKING: if (TEMP2 .GT. TEMP1) then
            t = GAMMA*t
          else
            !* l_inf_norm(beta-beta_new)
            TEMP1 = maxval(abs(VECTOR_TEMP1))
            TEMP2 = 0.0_rkdp
            
            !* l2_norm(beta)
            LOOP_COLUMNS_2: do j=1_ik4,p
              TEMP2 = TEMP2 + VECTOR_BETA(j)*VECTOR_BETA(j)
            end do LOOP_COLUMNS_2
            TEMP2 = sqrt(TEMP2)*EPSILON_RELATIVE
            
            CHECK_ACCURACY: if (TEMP1 .LE. TEMP2) then
              ACCURACY_REACHED = .TRUE.
            end if CHECK_ACCURACY
            
            !* beta=beta_new
            LOOP_UPDATE: do j=1_ik4,p
              VECTOR_BETA(j) = VECTOR_BETA_NEW(j)
            end do LOOP_UPDATE
            CRITERION_FULFILLED = .TRUE.
          end if CHECK_BACKTRACKING
          call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_TEMP1)
          call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_TEMP2)
        end do LOOP_CRITERION
        COUNTER = COUNTER + 1_ik4
      end do LOOP_ACCURACY_ITERATION
      
      
      !****************************************************
      !**     Write the results into the output files:   **
      !****************************************************
      CHECK_WHICH_ITERATION: if (i .EQ. 1_ik4) then
        call ROUTINE_WRITE_MATRIX_REAL(OUTPUT_FILE_NAME_BETA, 1_ik4, p, VECTOR_BETA, SHOW_INFO)
        call ROUTINE_WRITE_VALUE_INTEGER(OUTPUT_FILE_NAME_ITERATIONS, (COUNTER - 1_ik4), SHOW_INFO)
      else
        call ROUTINE_APPEND_MATRIX_REAL(OUTPUT_FILE_NAME_BETA, 1_ik4, p, VECTOR_BETA, SHOW_INFO)
        call ROUTINE_APPEND_VALUE_INTEGER(OUTPUT_FILE_NAME_ITERATIONS, (COUNTER - 1_ik4), SHOW_INFO)
      end if CHECK_WHICH_ITERATION
    end do LOOP_LAMBDA
    
    call ROUTINE_WRITE_VALUE_REAL(OUTPUT_FILE_NAME_LAMBDA_MAX, LAMBDA_MAX, SHOW_INFO)
    
    
    !******************************************************
    !**     Deallocate memory:                           **
    !******************************************************
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_BETA_NEW)
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_GRADIENT)
  end subroutine ROUTINE_GROUP_LASSO_PROX_GRAD_BACK_WARM
end module MODULE_TOP_GROUP_LASSO