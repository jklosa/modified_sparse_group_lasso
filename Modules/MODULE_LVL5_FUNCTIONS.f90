!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- MODULE_LVL5_FUNCTIONS.f90 ----              **
!**                                                      **
!**     This module contains the following subroutines   **
!**     for matrix algebra:                              **
!**       - ROUTINE_LAMBDA_MAX_LASSO                     **
!**       - ROUTINE_LAMBDA_MAX_GROUP_LASSO               **
!**       - ROUTINE_LAMBDA_MAX_SPARSE_GROUP_LASSO        **
!**       - ROUTINE_LAMBDA_MAX_MODIFIED_SG_LASSO         **
!**       - ROUTINE_SOFT_THRESHOLDING_SCALAR             **
!**       - ROUTINE_SOFT_THRESHOLDING_VECTOR             **
!**       - ROUTINE_SOFT_SCALING_SCALAR                  **
!**       - ROUTINE_HALF_L2_SQUARE_LOSS_FUNCTION         **
!**       - ROUTINE_TILDE_G_EPSILON                      **
!**       - ROUTINE_G_EPSILON_LOSS_FUNCTION              **
!**       - ROUTINE_POLYNOMIAL_LAMBDA                    **
!**       - ROUTINE_BISECTION_POL_LAMBDA                 **
!**       - ROUTINE_SCALED_GRADIENT_G_EPSILON            **
!**                                                      **
!**     This module uses the following modules:          **
!**       - MODULE_LVL1_KIND_NUMBERS                     **
!**       - MODULE_LVL2_ON_SCREEN_MESSAGES               **
!**       - MODULE_LVL3_MEMORY_MANAGEMENT                **
!**       - MODULE_LVL4_MATRIX_OPERATIONS                **
!**                                                      **
!**     LEIBNIZ INSTITUTE (FBN)                          **
!**     Dummerstorf                                      **
!**     Start: 14 March 2018                             **
!**     Author: Jan Klosa                                **
!**                                                      **
!**********************************************************
!**********************************************************

module MODULE_LVL5_FUNCTIONS
  use MODULE_LVL1_KIND_NUMBERS
  use MODULE_LVL2_ON_SCREEN_MESSAGES
  use MODULE_LVL3_MEMORY_MANAGEMENT
  use MODULE_LVL4_MATRIX_OPERATIONS
  implicit none
  
  contains
  
  
!**********************************************************
!**     Subroutine to determine the maximal value of     **
!**     lambda for the lasso:                            **
!**********************************************************
  subroutine ROUTINE_LAMBDA_MAX_LASSO(n, p, LAMBDA_MAX, VECTOR_Y_SIM, VECTOR_OMEGA_FEATURE, VECTOR_BETA, MATRIX_X_SIM)
    implicit none
    
    integer(ik4), intent(in)                :: n, p
    integer(ik4), save                      :: NUMBER_ZEROS_FEATURE
    integer(ik4)                            :: i, j, COUNTER, INFO
    real(rkdp), intent(inout)               :: LAMBDA_MAX
    real(rkdp), dimension(n), intent(in)    :: VECTOR_Y_SIM
    real(rkdp), dimension(p), intent(in)    :: VECTOR_OMEGA_FEATURE
    real(rkdp), dimension(p), intent(inout) :: VECTOR_BETA
    real(rkdp), allocatable, dimension(:)   :: VECTOR_ACTIVE_RESIDUAL, VECTOR_ACTIVE_BETA
    real(rkdp), allocatable, dimension(:)   :: VECTOR_ACTIVE_X_TRANSP_Y, VECTOR_ACTIVE_X_TRANSP_RESIDUAL
    real(rkdp), dimension(n,p), intent(in)  :: MATRIX_X_SIM
    real(rkdp), allocatable, dimension(:,:) :: MATRIX_ACTIVE_X, MATRIX_ACTIVE_X_TRANSP_X
    real(rkdp), allocatable, dimension(:,:) :: MATRIX_ACTIVE_X_TRANSP_X_INV
    character(len=40)                       :: NAME_VARIABLE
        
    NUMBER_ZEROS_FEATURE = 0_ik4
    i                    = 0_ik4
    j                    = 0_ik4
    COUNTER              = 0_ik4
    INFO                 = 0_ik4
    NAME_VARIABLE        = ''
    
    !* Determine the number of weights equal to zero
    LOOP_CHECK_WEIGHTS: do j=1_ik4,p
    	CHECK_WEIGHTS_ZERO_1: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
    		NUMBER_ZEROS_FEATURE = NUMBER_ZEROS_FEATURE + 1_ik4
    	end if CHECK_WEIGHTS_ZERO_1
    end do LOOP_CHECK_WEIGHTS
    
    
    !******************************************************
    !**     Allocate memory:                             **
    !******************************************************
    call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    
    !* Treatment, if unpenalized features are involved
    CHECK_HOW_MANY_ZERO: if (NUMBER_ZEROS_FEATURE .NE. 0_ik4) then
      call ROUTINE_ALLOCATE_VECTOR_REAL(n, VECTOR_ACTIVE_RESIDUAL)
      call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_ZEROS_FEATURE, VECTOR_ACTIVE_BETA)
      call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_ZEROS_FEATURE, VECTOR_ACTIVE_X_TRANSP_Y)
      call ROUTINE_ALLOCATE_MATRIX_REAL(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X)
      call ROUTINE_ALLOCATE_MATRIX_REAL(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X)
      call ROUTINE_ALLOCATE_MATRIX_REAL(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X_INV)
      
      
      !****************************************************
      !**     Calculations with "active" set:            **
      !****************************************************
      COUNTER = 1_ik4
      
      !* Determine the "active" set and create X_A
      LOOP_FILL_ACTIVE_X_COLUMNS: do j=1_ik4,p
      	CHECK_WEIGHTS_ZERO_2: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
    	  	LOOP_FILL_ACTIVE_X_ROWS: do i=1_ik4,n
    		  	MATRIX_ACTIVE_X(i,COUNTER) = MATRIX_X_SIM(i,j)
    		  end do LOOP_FILL_ACTIVE_X_ROWS
    		  COUNTER = COUNTER + 1_ik4
    	  end if CHECK_WEIGHTS_ZERO_2
      end do LOOP_FILL_ACTIVE_X_COLUMNS
      
      !* Calculate t(X_A)*y
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, VECTOR_Y_SIM, &
           & VECTOR_ACTIVE_X_TRANSP_Y)
      
      !* Calculate t(X_A)*X_A
      call ROUTINE_MATRIX_TRANSP_MATRIX_MULT_DGEMM(NUMBER_ZEROS_FEATURE, n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, &
           & MATRIX_ACTIVE_X, MATRIX_ACTIVE_X_TRANSP_X)
      
      !* Calculate inverse of t(X_A)*X_A
      call ROUTINE_MATRIX_INVERSION_DSYTRI2X(NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X, MATRIX_ACTIVE_X_TRANSP_X_INV, &
           & INFO)
      
      !* Calculate beta_A = [(t(X_A)*X_A)^(-1)]*[t(X_A)*y]
      call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X_INV, &
           & VECTOR_ACTIVE_X_TRANSP_Y, VECTOR_ACTIVE_BETA)
      
      !* Create beta with beta_A
      COUNTER = 1_ik4
      LOOP_INIT_BETA: do j=1_ik4,p
  	    CHECK_WEIGHTS_ZERO_3: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
  		    VECTOR_BETA(j) = VECTOR_ACTIVE_BETA(COUNTER)
  		    COUNTER = COUNTER + 1_ik4
  	    end if CHECK_WEIGHTS_ZERO_3
      end do LOOP_INIT_BETA
      
      !* Calculate X_A*beta_A
      call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, VECTOR_ACTIVE_BETA, &
           & VECTOR_ACTIVE_RESIDUAL)
      
      !* Create res_A = y - X_A*beta_A
      LOOP_ACTIVE_RESIDUAL: do i=1_ik4,n
      	VECTOR_ACTIVE_RESIDUAL(i) = VECTOR_Y_SIM(i) - VECTOR_ACTIVE_RESIDUAL(i)
      end do LOOP_ACTIVE_RESIDUAL
      
      !* Calculate t(X)*res_A
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, p, MATRIX_X_SIM, VECTOR_ACTIVE_RESIDUAL, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
      
      !****************************************************
      !**     Deallocate memory:                         **
      !****************************************************
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_RESIDUAL)
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_BETA)
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_X_TRANSP_Y)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X_TRANSP_X)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X_TRANSP_X_INV)
      
    !* Treatment, if only penalized features are involved
    else
    	!* Calculate t(X)*y
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, p, MATRIX_X_SIM, VECTOR_Y_SIM, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    end if CHECK_HOW_MANY_ZERO
    
    !* Scale t(X)*res_A with omega*n, if omega>0
    LOOP_ACTIVE_X_TRANSP_RESIDUAL: do j=1_ik4,p
    	CHECK_WEIGHTS_ZERO_4: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
  	  	VECTOR_ACTIVE_X_TRANSP_RESIDUAL(j) = 0.0_rkdp
  	  else
  		  VECTOR_ACTIVE_X_TRANSP_RESIDUAL(j) = VECTOR_ACTIVE_X_TRANSP_RESIDUAL(j)/(VECTOR_OMEGA_FEATURE(j)*real(n,rkdp))
  	  end if CHECK_WEIGHTS_ZERO_4
    end do LOOP_ACTIVE_X_TRANSP_RESIDUAL
    
    !* Determine lambda_max and do numerical correction
    LAMBDA_MAX = maxval(abs(VECTOR_ACTIVE_X_TRANSP_RESIDUAL))
    LAMBDA_MAX = LAMBDA_MAX*1.00001_rkdp
    
    
    !******************************************************
    !**     Deallocate memory:                           **
    !******************************************************
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
  end subroutine ROUTINE_LAMBDA_MAX_LASSO
  
  
!**********************************************************
!**     Subroutine to determine the maximal value of     **
!**     lambda for the group lasso:                      **
!**********************************************************
  subroutine ROUTINE_LAMBDA_MAX_GROUP_LASSO(n, p, NUMBER_GROUPS, LAMBDA_MAX, VECTOR_GROUP_SIZES, VECTOR_INDEX_START, &
             & VECTOR_INDEX_END, VECTOR_Y_SIM, VECTOR_OMEGA_GROUP, VECTOR_OMEGA_FEATURE, VECTOR_BETA, MATRIX_X_SIM)
    implicit none
    
    integer(ik4), intent(in)                           :: n, p, NUMBER_GROUPS
    integer(ik4), save                                 :: NUMBER_ZEROS_FEATURE
    integer(ik4)                                       :: i, j, k, COUNTER, INFO
    integer(ik4), dimension(NUMBER_GROUPS), intent(in) :: VECTOR_GROUP_SIZES, VECTOR_INDEX_START, VECTOR_INDEX_END
    real(rkdp), intent(inout)                          :: LAMBDA_MAX
    real(rkdp)                                         :: TEMP
    real(rkdp), dimension(n), intent(in)               :: VECTOR_Y_SIM
    real(rkdp), dimension(NUMBER_GROUPS), intent(in)   :: VECTOR_OMEGA_GROUP
    real(rkdp), dimension(p), intent(in)               :: VECTOR_OMEGA_FEATURE
    real(rkdp), dimension(p), intent(inout)            :: VECTOR_BETA
    real(rkdp), allocatable, dimension(:)              :: VECTOR_ACTIVE_RESIDUAL, VECTOR_ACTIVE_BETA
    real(rkdp), allocatable, dimension(:)              :: VECTOR_ACTIVE_X_TRANSP_Y, VECTOR_ACTIVE_X_TRANSP_RESIDUAL
    real(rkdp), allocatable, dimension(:)              :: VECTOR_L2_NORM_GROUPS
    real(rkdp), dimension(n,p), intent(in)             :: MATRIX_X_SIM
    real(rkdp), allocatable, dimension(:,:)            :: MATRIX_ACTIVE_X, MATRIX_ACTIVE_X_TRANSP_X
    real(rkdp), allocatable, dimension(:,:)            :: MATRIX_ACTIVE_X_TRANSP_X_INV
    character(len=40)                                  :: NAME_VARIABLE
        
    NUMBER_ZEROS_FEATURE = 0_ik4
    i                    = 0_ik4
    j                    = 0_ik4
    k                    = 0_ik4
    COUNTER              = 0_ik4
    INFO                 = 0_ik4
    TEMP                 = 0.0_rkdp
    NAME_VARIABLE        = ''
    
    !* Determine the number of weights equal to zero
    LOOP_CHECK_WEIGHTS: do j=1_ik4,p
    	CHECK_WEIGHTS_ZERO_1: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
    		NUMBER_ZEROS_FEATURE = NUMBER_ZEROS_FEATURE + 1_ik4
    	end if CHECK_WEIGHTS_ZERO_1
    end do LOOP_CHECK_WEIGHTS
    
    
    !******************************************************
    !**     Allocate memory:                             **
    !******************************************************
    call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_GROUPS, VECTOR_L2_NORM_GROUPS)
    
    !* Treatment, if unpenalized features are involved
    CHECK_HOW_MANY_ZERO: if (NUMBER_ZEROS_FEATURE .NE. 0_ik4) then
      call ROUTINE_ALLOCATE_VECTOR_REAL(n, VECTOR_ACTIVE_RESIDUAL)
      call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_ZEROS_FEATURE, VECTOR_ACTIVE_BETA)
      call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_ZEROS_FEATURE, VECTOR_ACTIVE_X_TRANSP_Y)
      call ROUTINE_ALLOCATE_MATRIX_REAL(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X)
      call ROUTINE_ALLOCATE_MATRIX_REAL(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X)
      call ROUTINE_ALLOCATE_MATRIX_REAL(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X_INV)
      
      
      !****************************************************
      !**     Calculations with "active" set:            **
      !****************************************************
      COUNTER = 1_ik4
      
      !* Determine the "active" set and create X_A
      LOOP_FILL_ACTIVE_X_COLUMNS: do j=1_ik4,p
      	CHECK_WEIGHTS_ZERO_2: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
    	  	LOOP_FILL_ACTIVE_X_ROWS: do i=1_ik4,n
    		  	MATRIX_ACTIVE_X(i,COUNTER) = MATRIX_X_SIM(i,j)
    		  end do LOOP_FILL_ACTIVE_X_ROWS
    		  COUNTER = COUNTER + 1_ik4
    	  end if CHECK_WEIGHTS_ZERO_2
      end do LOOP_FILL_ACTIVE_X_COLUMNS
      
      !* Calculate t(X_A)*y
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, VECTOR_Y_SIM, &
           & VECTOR_ACTIVE_X_TRANSP_Y)
      
      !* Calculate t(X_A)*X_A
      call ROUTINE_MATRIX_TRANSP_MATRIX_MULT_DGEMM(NUMBER_ZEROS_FEATURE, n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, &
           & MATRIX_ACTIVE_X, MATRIX_ACTIVE_X_TRANSP_X)
      
      !* Calculate inverse of t(X_A)*X_A
      call ROUTINE_MATRIX_INVERSION_DSYTRI2X(NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X, MATRIX_ACTIVE_X_TRANSP_X_INV, &
           & INFO)
      
      !* Calculate beta_A = [(t(X_A)*X_A)^(-1)]*[t(X_A)*y]
      call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X_INV, &
           & VECTOR_ACTIVE_X_TRANSP_Y, VECTOR_ACTIVE_BETA)
      
      !* Create beta with beta_A
      COUNTER = 1_ik4
      LOOP_INIT_BETA: do j=1_ik4,p
  	    CHECK_WEIGHTS_ZERO_3: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
  		    VECTOR_BETA(j) = VECTOR_ACTIVE_BETA(COUNTER)
  		    COUNTER = COUNTER + 1_ik4
  	    end if CHECK_WEIGHTS_ZERO_3
      end do LOOP_INIT_BETA
      
      !* Calculate X_A*beta_A
      call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, VECTOR_ACTIVE_BETA, &
           & VECTOR_ACTIVE_RESIDUAL)
      
      !* Create res_A = y - X_A*beta_A
      LOOP_ACTIVE_RESIDUAL: do i=1_ik4,n
      	VECTOR_ACTIVE_RESIDUAL(i) = VECTOR_Y_SIM(i) - VECTOR_ACTIVE_RESIDUAL(i)
      end do LOOP_ACTIVE_RESIDUAL
      
      !* Calculate t(X)*res_A
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, p, MATRIX_X_SIM, VECTOR_ACTIVE_RESIDUAL, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
      
      !****************************************************
      !**     Deallocate memory:                         **
      !****************************************************
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_RESIDUAL)
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_BETA)
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_X_TRANSP_Y)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X_TRANSP_X)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X_TRANSP_X_INV)
      
    !* Treatment, if only penalized features are involved
    else
    	!* Calculate t(X)*y
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, p, MATRIX_X_SIM, VECTOR_Y_SIM, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    end if CHECK_HOW_MANY_ZERO
    
    !* Scale t(X)*res_A groupwise with n*omega^G_k*sqrt(group_size_k), if omega^G_k>0
    !* Calculate groupwise l2-norm
    LOOP_ACTIVE_X_TRANSP_RESIDUAL: do k=1_ik4,NUMBER_GROUPS
    	CHECK_WEIGHTS_ZERO_4: if (VECTOR_OMEGA_GROUP(k) .EQ. 0.0_rkdp) then
  	  	VECTOR_L2_NORM_GROUPS(k) = 0.0_rkdp
  	  else
  	  	TEMP = real(n,rkdp)*VECTOR_OMEGA_GROUP(k)*sqrt(real(VECTOR_GROUP_SIZES(k),rkdp))
  	  	LOOP_GROUP_k: do j=VECTOR_INDEX_START(k),VECTOR_INDEX_END(k)
  		    VECTOR_L2_NORM_GROUPS(k) = VECTOR_L2_NORM_GROUPS(k) + VECTOR_ACTIVE_X_TRANSP_RESIDUAL(j)* &
  		    & VECTOR_ACTIVE_X_TRANSP_RESIDUAL(j)
  	  	end do LOOP_GROUP_k
  	  	VECTOR_L2_NORM_GROUPS(k) = sqrt(VECTOR_L2_NORM_GROUPS(k))/TEMP
  	  end if CHECK_WEIGHTS_ZERO_4
    end do LOOP_ACTIVE_X_TRANSP_RESIDUAL
    
    !* Determine lambda_max and do numerical correction
    LAMBDA_MAX = maxval(abs(VECTOR_L2_NORM_GROUPS))
    LAMBDA_MAX = LAMBDA_MAX*1.00001_rkdp
    
    
    !******************************************************
    !**     Deallocate memory:                           **
    !******************************************************
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_L2_NORM_GROUPS)
  end subroutine ROUTINE_LAMBDA_MAX_GROUP_LASSO
  
  
!**********************************************************
!**     Subroutine to determine the maximal value of     **
!**     lambda for the sparse-group lasso:               **
!**********************************************************
  subroutine ROUTINE_LAMBDA_MAX_SPARSE_GROUP_LASSO(n, p, NUMBER_GROUPS, ALPHA, LAMBDA_MAX, VECTOR_GROUP_SIZES, &
             & VECTOR_INDEX_START, VECTOR_INDEX_END, VECTOR_Y_SIM, VECTOR_OMEGA_GROUP, VECTOR_OMEGA_FEATURE, &
             & VECTOR_BETA, MATRIX_X_SIM)
    implicit none
    
    integer(ik4), intent(in)                           :: n, p, NUMBER_GROUPS
    integer(ik4), save                                 :: NUMBER_ZEROS_FEATURE
    integer(ik4)                                       :: i, j, k, COUNTER, INFO
    integer(ik4), dimension(NUMBER_GROUPS), intent(in) :: VECTOR_GROUP_SIZES, VECTOR_INDEX_START, VECTOR_INDEX_END
    real(rkdp), intent(in)                             :: ALPHA
    real(rkdp), intent(inout)                          :: LAMBDA_MAX
    real(rkdp)                                         :: TEMP_LEFT, TEMP_RIGHT
    real(rkdp), dimension(n), intent(in)               :: VECTOR_Y_SIM
    real(rkdp), dimension(NUMBER_GROUPS), intent(in)   :: VECTOR_OMEGA_GROUP
    real(rkdp), dimension(p), intent(in)               :: VECTOR_OMEGA_FEATURE
    real(rkdp), dimension(p), intent(inout)            :: VECTOR_BETA
    real(rkdp), allocatable, dimension(:)              :: VECTOR_ACTIVE_RESIDUAL, VECTOR_ACTIVE_BETA
    real(rkdp), allocatable, dimension(:)              :: VECTOR_ACTIVE_X_TRANSP_Y, VECTOR_ACTIVE_X_TRANSP_RESIDUAL
    real(rkdp), allocatable, dimension(:)              :: VECTOR_MAX_GROUPS, VECTOR_TEMP
    real(rkdp), dimension(n,p), intent(in)             :: MATRIX_X_SIM
    real(rkdp), allocatable, dimension(:,:)            :: MATRIX_ACTIVE_X, MATRIX_ACTIVE_X_TRANSP_X
    real(rkdp), allocatable, dimension(:,:)            :: MATRIX_ACTIVE_X_TRANSP_X_INV
    character(len=40)                                  :: NAME_VARIABLE
        
    NUMBER_ZEROS_FEATURE = 0_ik4
    i                    = 0_ik4
    j                    = 0_ik4
    k                    = 0_ik4
    COUNTER              = 0_ik4
    INFO                 = 0_ik4
    TEMP_LEFT            = 0.0_rkdp
    TEMP_RIGHT           = 0.0_rkdp
    NAME_VARIABLE        = ''
    
    !* Determine the number of weights equal to zero
    LOOP_CHECK_WEIGHTS: do j=1_ik4,p
    	CHECK_WEIGHTS_ZERO_1: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
    		NUMBER_ZEROS_FEATURE = NUMBER_ZEROS_FEATURE + 1_ik4
    	end if CHECK_WEIGHTS_ZERO_1
    end do LOOP_CHECK_WEIGHTS
    
    
    !******************************************************
    !**     Allocate memory:                             **
    !******************************************************
    call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_GROUPS, VECTOR_MAX_GROUPS)
    
    !* Treatment, if unpenalized features are involved
    CHECK_HOW_MANY_ZERO: if (NUMBER_ZEROS_FEATURE .NE. 0_ik4) then
      call ROUTINE_ALLOCATE_VECTOR_REAL(n, VECTOR_ACTIVE_RESIDUAL)
      call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_ZEROS_FEATURE, VECTOR_ACTIVE_BETA)
      call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_ZEROS_FEATURE, VECTOR_ACTIVE_X_TRANSP_Y)
      call ROUTINE_ALLOCATE_MATRIX_REAL(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X)
      call ROUTINE_ALLOCATE_MATRIX_REAL(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X)
      call ROUTINE_ALLOCATE_MATRIX_REAL(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X_INV)
      
      
      !****************************************************
      !**     Calculations with "active" set:            **
      !****************************************************
      COUNTER = 1_ik4
      
      !* Determine the "active" set and create X_A
      LOOP_FILL_ACTIVE_X_COLUMNS: do j=1_ik4,p
      	CHECK_WEIGHTS_ZERO_2: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
    	  	LOOP_FILL_ACTIVE_X_ROWS: do i=1_ik4,n
    		  	MATRIX_ACTIVE_X(i,COUNTER) = MATRIX_X_SIM(i,j)
    		  end do LOOP_FILL_ACTIVE_X_ROWS
    		  COUNTER = COUNTER + 1_ik4
    	  end if CHECK_WEIGHTS_ZERO_2
      end do LOOP_FILL_ACTIVE_X_COLUMNS
      
      !* Calculate t(X_A)*y
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, VECTOR_Y_SIM, &
           & VECTOR_ACTIVE_X_TRANSP_Y)
      
      !* Calculate t(X_A)*X_A
      call ROUTINE_MATRIX_TRANSP_MATRIX_MULT_DGEMM(NUMBER_ZEROS_FEATURE, n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, &
           & MATRIX_ACTIVE_X, MATRIX_ACTIVE_X_TRANSP_X)
      
      !* Calculate inverse of t(X_A)*X_A
      call ROUTINE_MATRIX_INVERSION_DSYTRI2X(NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X, MATRIX_ACTIVE_X_TRANSP_X_INV, &
           & INFO)
      
      !* Calculate beta_A = [(t(X_A)*X_A)^(-1)]*[t(X_A)*y]
      call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X_INV, &
           & VECTOR_ACTIVE_X_TRANSP_Y, VECTOR_ACTIVE_BETA)
      
      !* Create beta with beta_A
      COUNTER = 1_ik4
      LOOP_INIT_BETA: do j=1_ik4,p
  	    CHECK_WEIGHTS_ZERO_3: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
  		    VECTOR_BETA(j) = VECTOR_ACTIVE_BETA(COUNTER)
  		    COUNTER = COUNTER + 1_ik4
  	    end if CHECK_WEIGHTS_ZERO_3
      end do LOOP_INIT_BETA
      
      !* Calculate X_A*beta_A
      call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, VECTOR_ACTIVE_BETA, &
           & VECTOR_ACTIVE_RESIDUAL)
      
      !* Create res_A = y - X_A*beta_A
      LOOP_ACTIVE_RESIDUAL: do i=1_ik4,n
      	VECTOR_ACTIVE_RESIDUAL(i) = VECTOR_Y_SIM(i) - VECTOR_ACTIVE_RESIDUAL(i)
      end do LOOP_ACTIVE_RESIDUAL
      
      !* Calculate t(X)*res_A
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, p, MATRIX_X_SIM, VECTOR_ACTIVE_RESIDUAL, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
      
      !****************************************************
      !**     Deallocate memory:                         **
      !****************************************************
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_RESIDUAL)
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_BETA)
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_X_TRANSP_Y)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X_TRANSP_X)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X_TRANSP_X_INV)
      
    !* Treatment, if only penalized features are involved
    else
    	!* Calculate t(X)*y
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, p, MATRIX_X_SIM, VECTOR_Y_SIM, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    end if CHECK_HOW_MANY_ZERO
    
    !* Scale t(X)*res_A groupwise with n*omega^G_k*sqrt(group_size_k), if omega^G_k>0
    !* Calculate groupwise l2-norm
    LOOP_ACTIVE_X_TRANSP_RESIDUAL: do k=1_ik4,NUMBER_GROUPS
    	CHECK_WEIGHTS_ZERO_4: if (VECTOR_OMEGA_GROUP(k) .EQ. 0.0_rkdp) then
  	  	VECTOR_MAX_GROUPS(k) = 0.0_rkdp
  	  else
  	  	call ROUTINE_ALLOCATE_VECTOR_REAL(VECTOR_GROUP_SIZES(k), VECTOR_TEMP)
  	  	
  	  	!* Create groupwise X^(k,T)*r_(A,LS)/(n*omega^G_k)
  	  	LOOP_FILL_TEMP: do j=1_ik4,VECTOR_GROUP_SIZES(k)
  	  		VECTOR_TEMP(j) = VECTOR_ACTIVE_X_TRANSP_RESIDUAL(VECTOR_INDEX_START(k)+j-1)
  	  		VECTOR_TEMP(j) = VECTOR_TEMP(j)/(real(n,rkdp)*VECTOR_OMEGA_GROUP(k))
  	  	end do LOOP_FILL_TEMP
  	  	
  	  	!* Determine left and right border for bisection
  	  	TEMP_LEFT  = 0.0_rkdp
  	  	TEMP_RIGHT = maxval(abs(VECTOR_TEMP))
  	  	TEMP_RIGHT = TEMP_RIGHT/ALPHA
  	  	
  	  	call ROUTINE_BISECTION_POL_LAMBDA(VECTOR_GROUP_SIZES(k), ALPHA, TEMP_LEFT, TEMP_RIGHT, VECTOR_TEMP, &
  	  	     & VECTOR_MAX_GROUPS(k))
  	  	
  	  	call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_TEMP)
  	  end if CHECK_WEIGHTS_ZERO_4
    end do LOOP_ACTIVE_X_TRANSP_RESIDUAL
    
    !* Determine lambda_max and do numerical correction
    LAMBDA_MAX = maxval(VECTOR_MAX_GROUPS)
    LAMBDA_MAX = LAMBDA_MAX*1.00001_rkdp
    
    
    !******************************************************
    !**     Deallocate memory:                           **
    !******************************************************
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_MAX_GROUPS)
  end subroutine ROUTINE_LAMBDA_MAX_SPARSE_GROUP_LASSO
  
  
!**********************************************************
!**     Subroutine to determine the maximal value of     **
!**     lambda for the modified sparse-group lasso:      **
!**********************************************************
  subroutine ROUTINE_LAMBDA_MAX_MODIFIED_SG_LASSO(n, p, NUMBER_GROUPS, ALPHA, LAMBDA_MAX, VECTOR_GROUP_SIZES, &
             & VECTOR_INDEX_START, VECTOR_INDEX_END, VECTOR_Y_SIM, VECTOR_OMEGA_GROUP, VECTOR_OMEGA_FEATURE, &
             & VECTOR_BETA, MATRIX_X_SIM)
    implicit none
    
    integer(ik4), intent(in)                           :: n, p, NUMBER_GROUPS
    integer(ik4), save                                 :: NUMBER_ZEROS_FEATURE
    integer(ik4)                                       :: i, j, k, COUNTER, INFO
    integer(ik4), dimension(NUMBER_GROUPS), intent(in) :: VECTOR_GROUP_SIZES, VECTOR_INDEX_START, VECTOR_INDEX_END
    real(rkdp), intent(in)                             :: ALPHA
    real(rkdp), intent(inout)                          :: LAMBDA_MAX
    real(rkdp)                                         :: TEMP_LEFT, TEMP_RIGHT
    real(rkdp), dimension(n), intent(in)               :: VECTOR_Y_SIM
    real(rkdp), dimension(NUMBER_GROUPS), intent(in)   :: VECTOR_OMEGA_GROUP
    real(rkdp), dimension(p), intent(in)               :: VECTOR_OMEGA_FEATURE
    real(rkdp), dimension(p), intent(inout)            :: VECTOR_BETA
    real(rkdp), allocatable, dimension(:)              :: VECTOR_ACTIVE_RESIDUAL, VECTOR_ACTIVE_BETA
    real(rkdp), allocatable, dimension(:)              :: VECTOR_ACTIVE_X_TRANSP_Y, VECTOR_ACTIVE_X_TRANSP_RESIDUAL
    real(rkdp), allocatable, dimension(:)              :: VECTOR_MAX_GROUPS, VECTOR_TEMP
    real(rkdp), dimension(n,p), intent(in)             :: MATRIX_X_SIM
    real(rkdp), allocatable, dimension(:,:)            :: MATRIX_ACTIVE_X, MATRIX_ACTIVE_X_TRANSP_X
    real(rkdp), allocatable, dimension(:,:)            :: MATRIX_ACTIVE_X_TRANSP_X_INV
    character(len=40)                                  :: NAME_VARIABLE
        
    NUMBER_ZEROS_FEATURE = 0_ik4
    i                    = 0_ik4
    j                    = 0_ik4
    k                    = 0_ik4
    COUNTER              = 0_ik4
    INFO                 = 0_ik4
    TEMP_LEFT            = 0.0_rkdp
    TEMP_RIGHT           = 0.0_rkdp
    NAME_VARIABLE        = ''
    
    !* Determine the number of weights equal to zero
    LOOP_CHECK_WEIGHTS: do j=1_ik4,p
    	CHECK_WEIGHTS_ZERO_1: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
    		NUMBER_ZEROS_FEATURE = NUMBER_ZEROS_FEATURE + 1_ik4
    	end if CHECK_WEIGHTS_ZERO_1
    end do LOOP_CHECK_WEIGHTS
    
    
    !******************************************************
    !**     Allocate memory:                             **
    !******************************************************
    call ROUTINE_ALLOCATE_VECTOR_REAL(p, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_GROUPS, VECTOR_MAX_GROUPS)
    
    !* Treatment, if unpenalized features are involved
    CHECK_HOW_MANY_ZERO: if (NUMBER_ZEROS_FEATURE .NE. 0_ik4) then
      call ROUTINE_ALLOCATE_VECTOR_REAL(n, VECTOR_ACTIVE_RESIDUAL)
      call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_ZEROS_FEATURE, VECTOR_ACTIVE_BETA)
      call ROUTINE_ALLOCATE_VECTOR_REAL(NUMBER_ZEROS_FEATURE, VECTOR_ACTIVE_X_TRANSP_Y)
      call ROUTINE_ALLOCATE_MATRIX_REAL(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X)
      call ROUTINE_ALLOCATE_MATRIX_REAL(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X)
      call ROUTINE_ALLOCATE_MATRIX_REAL(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X_INV)
      
      
      !****************************************************
      !**     Calculations with "active" set:            **
      !****************************************************
      COUNTER = 1_ik4
      
      !* Determine the "active" set and create X_A
      LOOP_FILL_ACTIVE_X_COLUMNS: do j=1_ik4,p
      	CHECK_WEIGHTS_ZERO_2: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
    	  	LOOP_FILL_ACTIVE_X_ROWS: do i=1_ik4,n
    		  	MATRIX_ACTIVE_X(i,COUNTER) = MATRIX_X_SIM(i,j)
    		  end do LOOP_FILL_ACTIVE_X_ROWS
    		  COUNTER = COUNTER + 1_ik4
    	  end if CHECK_WEIGHTS_ZERO_2
      end do LOOP_FILL_ACTIVE_X_COLUMNS
      
      !* Calculate t(X_A)*y
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, VECTOR_Y_SIM, &
           & VECTOR_ACTIVE_X_TRANSP_Y)
      
      !* Calculate t(X_A)*X_A
      call ROUTINE_MATRIX_TRANSP_MATRIX_MULT_DGEMM(NUMBER_ZEROS_FEATURE, n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, &
           & MATRIX_ACTIVE_X, MATRIX_ACTIVE_X_TRANSP_X)
      
      !* Calculate inverse of t(X_A)*X_A
      call ROUTINE_MATRIX_INVERSION_DSYTRI2X(NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X, MATRIX_ACTIVE_X_TRANSP_X_INV, &
           & INFO)
      
      !* Calculate beta_A = [(t(X_A)*X_A)^(-1)]*[t(X_A)*y]
      call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(NUMBER_ZEROS_FEATURE, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X_TRANSP_X_INV, &
           & VECTOR_ACTIVE_X_TRANSP_Y, VECTOR_ACTIVE_BETA)
      
      !* Create beta with beta_A
      COUNTER = 1_ik4
      LOOP_INIT_BETA: do j=1_ik4,p
  	    CHECK_WEIGHTS_ZERO_3: if (VECTOR_OMEGA_FEATURE(j) .EQ. 0.0_rkdp) then
  		    VECTOR_BETA(j) = VECTOR_ACTIVE_BETA(COUNTER)
  		    COUNTER = COUNTER + 1_ik4
  	    end if CHECK_WEIGHTS_ZERO_3
      end do LOOP_INIT_BETA
      
      !* Calculate X_A*beta_A
      call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(n, NUMBER_ZEROS_FEATURE, MATRIX_ACTIVE_X, VECTOR_ACTIVE_BETA, &
           & VECTOR_ACTIVE_RESIDUAL)
      
      !* Create res_A = y - X_A*beta_A
      LOOP_ACTIVE_RESIDUAL: do i=1_ik4,n
      	VECTOR_ACTIVE_RESIDUAL(i) = VECTOR_Y_SIM(i) - VECTOR_ACTIVE_RESIDUAL(i)
      end do LOOP_ACTIVE_RESIDUAL
      
      !* Calculate t(X)*res_A
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, p, MATRIX_X_SIM, VECTOR_ACTIVE_RESIDUAL, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
      
      !****************************************************
      !**     Deallocate memory:                         **
      !****************************************************
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_RESIDUAL)
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_BETA)
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_X_TRANSP_Y)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X_TRANSP_X)
      call ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX_ACTIVE_X_TRANSP_X_INV)
      
    !* Treatment, if only penalized features are involved
    else
    	!* Calculate t(X)*y
      call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, p, MATRIX_X_SIM, VECTOR_Y_SIM, VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    end if CHECK_HOW_MANY_ZERO
    
    !* Scale t(X)*res_A groupwise with alpha*n*omega^G_k, if omega^G_k>0
    LOOP_ACTIVE_X_TRANSP_RESIDUAL: do k=1_ik4,NUMBER_GROUPS
    	CHECK_WEIGHTS_ZERO_4: if (VECTOR_OMEGA_GROUP(k) .EQ. 0.0_rkdp) then
  	  	VECTOR_MAX_GROUPS(k) = 0.0_rkdp
  	  else
  	  	VECTOR_MAX_GROUPS(k) = maxval(abs(VECTOR_ACTIVE_X_TRANSP_RESIDUAL(VECTOR_INDEX_START(k):VECTOR_INDEX_END(k))))
  	  	VECTOR_MAX_GROUPS(k) = VECTOR_MAX_GROUPS(k)/(ALPHA*real(n,rkdp)*VECTOR_OMEGA_GROUP(k))
  	  end if CHECK_WEIGHTS_ZERO_4
    end do LOOP_ACTIVE_X_TRANSP_RESIDUAL
    
    !* Determine lambda_max and do numerical correction
    LAMBDA_MAX = maxval(VECTOR_MAX_GROUPS)
    LAMBDA_MAX = LAMBDA_MAX*1.00001_rkdp
    
    
    !******************************************************
    !**     Deallocate memory:                           **
    !******************************************************
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_ACTIVE_X_TRANSP_RESIDUAL)
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_MAX_GROUPS)
  end subroutine ROUTINE_LAMBDA_MAX_MODIFIED_SG_LASSO
  
  
!**********************************************************
!**     Subroutine which performs soft-thresholding:     **
!**********************************************************
  subroutine ROUTINE_SOFT_THRESHOLDING_SCALAR(ROWS, SCALAR_IN, VECTOR_IN, VECTOR_OUT)
    implicit none
    
    integer(ik4), intent(in)                   :: ROWS
    integer(ik4)                               :: i
    real(rkdp), intent(in)                     :: SCALAR_IN
    real(rkdp), dimension(ROWS), intent(in)    :: VECTOR_IN
    real(rkdp), dimension(ROWS), intent(inout) :: VECTOR_OUT
    
    i = 1_ik4
    LOOP_VECTOR_LENGTH: do i=1_ik4,ROWS
    	CHECK_THRESHOLD: if (VECTOR_IN(i) .GT. SCALAR_IN) then
    		VECTOR_OUT(i) = VECTOR_IN(i) - SCALAR_IN
    	else if (VECTOR_IN(i) .LT. (-SCALAR_IN)) then
    		VECTOR_OUT(i) = VECTOR_IN(i) + SCALAR_IN
    	else
    		VECTOR_OUT(i) = 0.0_rkdp
    	end if CHECK_THRESHOLD
    end do LOOP_VECTOR_LENGTH
  end subroutine ROUTINE_SOFT_THRESHOLDING_SCALAR
  
  
!**********************************************************
!**     Subroutine which performs soft-thresholding:     **
!**********************************************************
  subroutine ROUTINE_SOFT_THRESHOLDING_VECTOR(ROWS, VECTOR_IN1, VECTOR_IN2, VECTOR_OUT)
    implicit none
    
    integer(ik4), intent(in)                   :: ROWS
    integer(ik4)                               :: i
    real(rkdp), dimension(ROWS), intent(in)    :: VECTOR_IN1, VECTOR_IN2
    real(rkdp), dimension(ROWS), intent(inout) :: VECTOR_OUT
    
    i = 1_ik4
    LOOP_VECTOR_LENGTH: do i=1_ik4,ROWS
    	CHECK_THRESHOLD: if (VECTOR_IN1(i) .GT. VECTOR_IN2(i)) then
    		VECTOR_OUT(i) = VECTOR_IN1(i) - VECTOR_IN2(i)
    	else if (VECTOR_IN1(i) .LT. (-VECTOR_IN2(i))) then
    		VECTOR_OUT(i) = VECTOR_IN1(i) + VECTOR_IN2(i)
    	else
    		VECTOR_OUT(i) = 0.0_rkdp
    	end if CHECK_THRESHOLD
    end do LOOP_VECTOR_LENGTH
  end subroutine ROUTINE_SOFT_THRESHOLDING_VECTOR
  
  
!**********************************************************
!**     Subroutine which performs soft-scaling:          **
!**********************************************************
  subroutine ROUTINE_SOFT_SCALING_SCALAR(ROWS, SCALAR_IN, VECTOR_IN, VECTOR_OUT)
    implicit none
    
    integer(ik4), intent(in)                   :: ROWS
    integer(ik4)                               :: i
    real(rkdp), intent(in)                     :: SCALAR_IN
    real(rkdp)                                 :: L2_NORM_VECTOR_IN, SCALING
    real(rkdp), dimension(ROWS), intent(in)    :: VECTOR_IN
    real(rkdp), dimension(ROWS), intent(inout) :: VECTOR_OUT
    
    i                 = 1_ik4
    L2_NORM_VECTOR_IN = 0.0_rkdp
    SCALING           = 0.0_rkdp
    
    LOOP_L2_NORM: do i=1_ik4,ROWS
    	L2_NORM_VECTOR_IN = L2_NORM_VECTOR_IN + VECTOR_IN(i)*VECTOR_IN(i)
    end do LOOP_L2_NORM
    L2_NORM_VECTOR_IN = sqrt(L2_NORM_VECTOR_IN)
    
    CHECK_THRESHOLD: if (L2_NORM_VECTOR_IN .GT. SCALAR_IN) then
    	SCALING = 1.0_rkdp - SCALAR_IN/L2_NORM_VECTOR_IN
    	LOOP_VECTOR_LENGTH_1: do i=1_ik4,ROWS
    		VECTOR_OUT(i) = SCALING*VECTOR_IN(i)
    	end do LOOP_VECTOR_LENGTH_1
    else
    	LOOP_VECTOR_LENGTH_2: do i=1_ik4,ROWS
    		VECTOR_OUT(i) = 0.0_rkdp
    	end do LOOP_VECTOR_LENGTH_2
    end if CHECK_THRESHOLD
  end subroutine ROUTINE_SOFT_SCALING_SCALAR
  
  
!**********************************************************
!**     Subroutine which calculates the value of the     **
!**     following loss function:                         **
!**                                                      **
!**         0.5*l2_norm_squared(v1 - X%*%v2)             **
!**                                                      **
!**********************************************************
  subroutine ROUTINE_HALF_L2_SQUARE_LOSS_FUNCTION(ROWS, COLUMNS, VECTOR_IN1, VECTOR_IN2, MATRIX_IN, SCALAR_OUT)
    implicit none
    
    integer(ik4), intent(in)                        :: ROWS, COLUMNS
    integer(ik4)                                    :: i
    real(rkdp), intent(inout)                       :: SCALAR_OUT
    real(rkdp), dimension(ROWS), intent(in)         :: VECTOR_IN1
    real(rkdp), dimension(COLUMNS), intent(in)      :: VECTOR_IN2
    real(rkdp), allocatable, dimension(:)           :: VECTOR_TEMP
    real(rkdp), dimension(ROWS,COLUMNS), intent(in) :: MATRIX_IN
    
    i          = 1_ik4
    SCALAR_OUT = 0.0_rkdp
    
    call ROUTINE_ALLOCATE_VECTOR_REAL(ROWS, VECTOR_TEMP)
    call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(ROWS, COLUMNS, MATRIX_IN, VECTOR_IN2, VECTOR_TEMP)
    
    LOOP_VECTOR_LENGTH: do i=1_ik4,ROWS
    	VECTOR_TEMP(i) = VECTOR_IN1(i) - VECTOR_TEMP(i)
    	SCALAR_OUT     = SCALAR_OUT + VECTOR_TEMP(i)*VECTOR_TEMP(i)
    end do LOOP_VECTOR_LENGTH
    SCALAR_OUT = 0.5_rkdp*SCALAR_OUT
    
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_TEMP)
  end subroutine ROUTINE_HALF_L2_SQUARE_LOSS_FUNCTION
  
  
!**********************************************************
!**     Subroutine which calculates the value of the     **
!**     smoothed l2-norm "tilde_g_epsilon":              **
!**********************************************************
  subroutine ROUTINE_TILDE_G_EPSILON(n, EPSILON, VECTOR_IN, SCALAR_OUT)
    implicit none
    
    integer(ik4), intent(in)             :: n
    integer(ik4)                         :: i
    real(rkdp), intent(in)               :: EPSILON
    real(rkdp), intent(inout)            :: SCALAR_OUT
    real(rkdp)                           :: TEMP
    real(rkdp), dimension(n), intent(in) :: VECTOR_IN
    
    i    = 1_ik4
    TEMP = 0.0_rkdp
    
    LOOP_SQUARED_L2_NORM: do i=1_ik4,n
    	TEMP = TEMP + VECTOR_IN(i)*VECTOR_IN(i)
    end do LOOP_SQUARED_L2_NORM
    
    CHECK_WHICH_CASE: if (TEMP .GT. EPSILON) then
    	SCALAR_OUT = sqrt(TEMP)
    else
    	SCALAR_OUT = 0.5_rkdp*(TEMP/EPSILON + EPSILON)
    end if CHECK_WHICH_CASE
  end subroutine ROUTINE_TILDE_G_EPSILON
  
  
!**********************************************************
!**     Subroutine which calculates the value of the     **
!**     following loss function:                         **
!**                                                      **
!**         0.5*l2_norm_squared(y - X%*%beta) +          **
!**         sum_g_eps                                    **
!**                                                      **
!**********************************************************
  subroutine ROUTINE_G_EPSILON_LOSS_FUNCTION(n, p, NUMBER_GROUPS, ALPHA, LAMBDA, EPSILON, VECTOR_GROUP_SIZES, &
             & VECTOR_INDEX_START, VECTOR_INDEX_END, VECTOR_OMEGA_GROUP, VECTOR_Y, VECTOR_BETA, MATRIX_X, SCALAR_OUT)
    implicit none
    
    integer(ik4), intent(in)                           :: n, p, NUMBER_GROUPS
    integer(ik4), save                                 :: i, k
    integer(ik4), dimension(NUMBER_GROUPS), intent(in) :: VECTOR_GROUP_SIZES, VECTOR_INDEX_START, VECTOR_INDEX_END
    real(rkdp), intent(in)                             :: ALPHA, LAMBDA, EPSILON
    real(rkdp), intent(inout)                          :: SCALAR_OUT
    real(rkdp), save                                   :: TEMP, TEMP_SUM_G_EPS
    real(rkdp), dimension(NUMBER_GROUPS), intent(in)   :: VECTOR_OMEGA_GROUP
    real(rkdp), dimension(n), intent(in)               :: VECTOR_Y
    real(rkdp), dimension(p), intent(in)               :: VECTOR_BETA
    real(rkdp), allocatable, dimension(:)              :: VECTOR_TEMP
    real(rkdp), dimension(n,p), intent(in)             :: MATRIX_X
    
    i              = 1_ik4
    k              = 1_ik4
    TEMP           = 0.0_rkdp
    TEMP_SUM_G_EPS = 0.0_rkdp
    
    call ROUTINE_ALLOCATE_VECTOR_REAL(n, VECTOR_TEMP)
    
    LOOP_GROUPS: do k=1_ik4,NUMBER_GROUPS
    	!* Calculate X^(k)*beta(k)
    	call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(n, VECTOR_GROUP_SIZES(k), MATRIX_X(:,VECTOR_INDEX_START(k):VECTOR_INDEX_END(k)), &
    	     & VECTOR_BETA(VECTOR_INDEX_START(k):VECTOR_INDEX_END(k)), VECTOR_TEMP)
    	call ROUTINE_TILDE_G_EPSILON(n, EPSILON, VECTOR_TEMP, TEMP)
    	TEMP_SUM_G_EPS = TEMP_SUM_G_EPS + sqrt(real(VECTOR_GROUP_SIZES(k),rkdp))*VECTOR_OMEGA_GROUP(k)*TEMP
    end do LOOP_GROUPS
    TEMP_SUM_G_EPS = TEMP_SUM_G_EPS*real(n,rkdp)*(1.0_rkdp-ALPHA)*LAMBDA
    
    call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(n, p, MATRIX_X, VECTOR_BETA, VECTOR_TEMP)
    
    SCALAR_OUT = 0.0_rkdp
    LOOP_VECTOR_LENGTH: do i=1_ik4,n
    	VECTOR_TEMP(i) = VECTOR_Y(i) - VECTOR_TEMP(i)
    	SCALAR_OUT     = SCALAR_OUT + VECTOR_TEMP(i)*VECTOR_TEMP(i)
    end do LOOP_VECTOR_LENGTH
    SCALAR_OUT = 0.5_rkdp*SCALAR_OUT + TEMP_SUM_G_EPS
    
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_TEMP)
  end subroutine ROUTINE_G_EPSILON_LOSS_FUNCTION
  
  
!**********************************************************
!**     Subroutine which calculates the value of the     **
!**     following polynomial function in lambda:         **
!**                                                      **
!**       sum[(abs(v_j)-s1*s2)_+]^2 - (1-s1)^2*s2^2*rows **
!**                                                      **
!**       v  = X^(k,T)*r_(A,LS)/n , vector of len=rows   **
!**       s1 = alpha                                     **
!**       s2 = lambda                                    **
!**                                                      **
!**********************************************************
  subroutine ROUTINE_POLYNOMIAL_LAMBDA(ROWS, SCALAR_IN1, SCALAR_IN2, VECTOR_IN, SCALAR_OUT)
    implicit none
    
    integer(ik4), intent(in)                :: ROWS
    integer(ik4)                            :: i
    real(rkdp), intent(in)                  :: SCALAR_IN1, SCALAR_IN2
    real(rkdp)                              :: TEMP
    real(rkdp), intent(inout)               :: SCALAR_OUT
    real(rkdp), dimension(ROWS), intent(in) :: VECTOR_IN
    
    i          = 1_ik4
    TEMP       = 0.0_rkdp
    SCALAR_OUT = 0.0_rkdp
    
    LOOP_ROWS: do i=1_ik4,ROWS
    	TEMP = abs(VECTOR_IN(i)) - SCALAR_IN1*SCALAR_IN2
    	CHECK_TEMP: if (TEMP .GT. 0.0_rkdp) then
    		SCALAR_OUT = SCALAR_OUT + TEMP*TEMP
    	end if CHECK_TEMP
    end do LOOP_ROWS
    SCALAR_OUT = SCALAR_OUT - (1.0_rkdp-SCALAR_IN1)*(1.0_rkdp-SCALAR_IN1)*SCALAR_IN2*SCALAR_IN2*real(ROWS,rkdp)
  end subroutine ROUTINE_POLYNOMIAL_LAMBDA
  
  
!**********************************************************
!**     Subroutine to perform bisection on the previous  **
!**     polynomial:                                      **
!**********************************************************
  recursive subroutine ROUTINE_BISECTION_POL_LAMBDA(ROWS, SCALAR_IN, SCALAR_IN_LEFT, SCALAR_IN_RIGHT, VECTOR_IN, SCALAR_OUT)
    implicit none
    
    integer(ik4), intent(in)                :: ROWS
    real(rkdp), intent(in)                  :: SCALAR_IN, SCALAR_IN_LEFT, SCALAR_IN_RIGHT
    real(rkdp), intent(inout)               :: SCALAR_OUT
    real(rkdp)                              :: TOLERANCE, SCALAR_MID_POINT, FUNC_LEFT, FUNC_MID, FUNC_RIGHT
    real(rkdp), dimension(ROWS), intent(in) :: VECTOR_IN
    
    TOLERANCE        = 0.0000000000001_rkdp
    SCALAR_MID_POINT = 0.5_rkdp*(SCALAR_IN_LEFT+SCALAR_IN_RIGHT)
    FUNC_LEFT        = 0.0_rkdp
    FUNC_MID         = 0.0_rkdp
    FUNC_RIGHT       = 0.0_rkdp
    
    call ROUTINE_POLYNOMIAL_LAMBDA(ROWS, SCALAR_IN, SCALAR_IN_LEFT, VECTOR_IN, FUNC_LEFT)
    call ROUTINE_POLYNOMIAL_LAMBDA(ROWS, SCALAR_IN, SCALAR_MID_POINT, VECTOR_IN, FUNC_MID)
    call ROUTINE_POLYNOMIAL_LAMBDA(ROWS, SCALAR_IN, SCALAR_IN_RIGHT, VECTOR_IN, FUNC_RIGHT)
    
    CHECK_ROOT_INSIDE_INTERVAL: if (FUNC_LEFT*FUNC_MID .LT. 0.0_rkdp) then
    	CHECK_CLOSE_ENOUGH_1: if (abs(SCALAR_IN_LEFT-SCALAR_MID_POINT) .GT. TOLERANCE) then
    		call ROUTINE_BISECTION_POL_LAMBDA(ROWS, SCALAR_IN, SCALAR_IN_LEFT, SCALAR_MID_POINT, VECTOR_IN, SCALAR_OUT)
    	else
    		SCALAR_OUT = SCALAR_MID_POINT
    		return
    	end if CHECK_CLOSE_ENOUGH_1
    else if (FUNC_MID*FUNC_RIGHT .LT. 0.0_rkdp) then
    	CHECK_CLOSE_ENOUGH_2: if (abs(SCALAR_MID_POINT-SCALAR_IN_RIGHT) .GT. TOLERANCE) then
    		call ROUTINE_BISECTION_POL_LAMBDA(ROWS, SCALAR_IN, SCALAR_MID_POINT, SCALAR_IN_RIGHT, VECTOR_IN, SCALAR_OUT)
    	else
    		SCALAR_OUT = SCALAR_MID_POINT
    		return
    	end if CHECK_CLOSE_ENOUGH_2
    end if CHECK_ROOT_INSIDE_INTERVAL
  end subroutine ROUTINE_BISECTION_POL_LAMBDA
  
  
!**********************************************************
!**     Subroutine to calculate the scaled gradient of   **
!**     the smooth approximation of the l2-norm called   **
!**     g_epsilon:                                       **
!**********************************************************
  subroutine ROUTINE_SCALED_GRADIENT_G_EPSILON(n, p, NUMBER_GROUPS, ALPHA, LAMBDA, EPSILON, VECTOR_GROUP_SIZES, &
             & VECTOR_INDEX_START, VECTOR_INDEX_END, VECTOR_OMEGA_GROUP, VECTOR_BETA, MATRIX_X, &
             & VECTOR_SCALED_GRADIENT_G_EPS)
    implicit none
    
    integer(ik4), intent(in)                           :: n, p, NUMBER_GROUPS
    integer(ik4), save                                 :: j, k
    integer(ik4), dimension(NUMBER_GROUPS), intent(in) :: VECTOR_GROUP_SIZES, VECTOR_INDEX_START, VECTOR_INDEX_END
    real(rkdp), intent(in)                             :: ALPHA, LAMBDA, EPSILON
    real(rkdp), save                                   :: TEMP1, TEMP2
    real(rkdp), dimension(NUMBER_GROUPS), intent(in)   :: VECTOR_OMEGA_GROUP
    real(rkdp), dimension(p), intent(in)               :: VECTOR_BETA
    real(rkdp), dimension(p), intent(inout)            :: VECTOR_SCALED_GRADIENT_G_EPS
    real(rkdp), allocatable, dimension(:)              :: VECTOR_TEMP1, VECTOR_TEMP2
    real(rkdp), dimension(n,p), intent(in)             :: MATRIX_X
    
    j     = 1_ik4
    k     = 1_ik4
    TEMP1 = 0.0_rkdp
    TEMP2 = 0.0_rkdp
    
    call ROUTINE_ALLOCATE_VECTOR_REAL(n, VECTOR_TEMP1)
    
    LOOP_GROUPS: do k=1_ik4,NUMBER_GROUPS
      call ROUTINE_ALLOCATE_VECTOR_REAL(VECTOR_GROUP_SIZES(k), VECTOR_TEMP2)
      
    	!* Calculate X^(k)*beta^(k)
    	call ROUTINE_MATRIX_VECTOR_MULT_DGEMV(n, VECTOR_GROUP_SIZES(k), MATRIX_X(:,VECTOR_INDEX_START(k):VECTOR_INDEX_END(k)), &
    	     & VECTOR_BETA(VECTOR_INDEX_START(k):VECTOR_INDEX_END(k)), VECTOR_TEMP1)
    	
    	!* Calculate the l2-norm of X^(k)*beta^(k)
    	TEMP1 = 0.0_rkdp
    	LOOP_L2_NORM: do j=1_ik4,n
    		TEMP1 = TEMP1 + VECTOR_TEMP1(j)*VECTOR_TEMP1(j)
    	end do LOOP_L2_NORM
    	TEMP1 = sqrt(TEMP1)
    	
    	!* Calculate scaling factor n*(1-alpha)*lambda*sqrt(p_k)*omega^G_k
    	TEMP2 = real(n,rkdp)*(1.0_rkdp-ALPHA)*LAMBDA*sqrt(real(VECTOR_GROUP_SIZES(k),rkdp))*VECTOR_OMEGA_GROUP(k)
    	
    	!* Calculate t(X^(k))*X^(k)*beta(k)
    	call ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(n, VECTOR_GROUP_SIZES(k), &
    	     & MATRIX_X(:,VECTOR_INDEX_START(k):VECTOR_INDEX_END(k)), VECTOR_TEMP1, VECTOR_TEMP2)
    	
    	!* Calculate final result
    	CHECK_WHICH_CASE: if (TEMP1 .GT. EPSILON) then
    		LOOP_FILL_VECTOR_1: do j=1_ik4,VECTOR_GROUP_SIZES(k)
    		  VECTOR_SCALED_GRADIENT_G_EPS(VECTOR_INDEX_START(k)+j-1_ik4) = (TEMP2/TEMP1)*VECTOR_TEMP2(j)
    		end do LOOP_FILL_VECTOR_1
    	else
    		LOOP_FILL_VECTOR_2: do j=1_ik4,VECTOR_GROUP_SIZES(k)
    		  VECTOR_SCALED_GRADIENT_G_EPS(VECTOR_INDEX_START(k)+j-1_ik4) = (TEMP2/EPSILON)*VECTOR_TEMP2(j)
    		end do LOOP_FILL_VECTOR_2
    	end if CHECK_WHICH_CASE
    	
      call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_TEMP2)
    end do LOOP_GROUPS
    call ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR_TEMP1)
  end subroutine ROUTINE_SCALED_GRADIENT_G_EPSILON
end module MODULE_LVL5_FUNCTIONS