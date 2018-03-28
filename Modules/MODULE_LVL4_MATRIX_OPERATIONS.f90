!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- MODULE_LVL4_MATRIX_OPERATIONS.f90 ----      **
!**                                                      **
!**     This module contains the following subroutines   **
!**     for matrix algebra:                              **
!**       - ROUTINE_MATRIX_INVERSION_DGESV               **
!**       - ROUTINE_MATRIX_INVERSION_DSYTRI2X            **
!**       - ROUTINE_MATRIX_MATRIX_MULT_DGEMM             **
!**       - ROUTINE_MATRIX_TRANSP_MATRIX_MULT_DGEMM      **
!**       - ROUTINE_MATRIX_VECTOR_MULT_DGEMV             **
!**       - ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV      **
!**       - ROUTINE_CENTER_COLUMNS                       **
!**       - ROUTINE_CALCULATE_MEANS                      **
!**                                                      **
!**     This module uses the following modules:          **
!**       - MODULE_LVL1_KIND_NUMBERS                     **
!**       - MODULE_LVL2_ON_SCREEN_MESSAGES               **
!**       - MODULE_LVL3_MEMORY_MANAGEMENT                **
!**                                                      **
!**     LEIBNIZ INSTITUTE (FBN)                          **
!**     Dummerstorf                                      **
!**     Start: 13 March 2018                             **
!**     Author: Jan Klosa                                **
!**                                                      **
!**********************************************************
!**********************************************************

module MODULE_LVL4_MATRIX_OPERATIONS
  use MODULE_LVL1_KIND_NUMBERS
  use MODULE_LVL2_ON_SCREEN_MESSAGES
  use MODULE_LVL3_MEMORY_MANAGEMENT
  implicit none
  
  contains
  
  
!**********************************************************
!**     Subroutine to invert a matrix with the DGESV     **
!**     solver of LAPACK:                                **
!**                                                      **
!**     Structure of DGESV:                              **
!**       - DGESV(N, NRHS, A, LDA, IPIV, B, LDB, INFO)   **
!**                                                      **
!**     Used variables:                                  **
!**       - N    --> ROWS                                **
!**       - NRHS --> ROWS                                **
!**       - A    --> MATRIX                              **
!**       - LDA  --> ROWS                                **
!**       - IPIV --> IPIV                                **
!**       - B    --> INVERSE_MATRIX                      **
!**       - LDB  --> ROWS                                **
!**       - INFO --> INFO                                **
!**********************************************************
  subroutine ROUTINE_MATRIX_INVERSION_DGESV(ROWS, MATRIX, INVERSE_MATRIX, INFO)
    implicit none
    
    integer(ik4), intent(in)                        :: ROWS
    integer(ik4), intent(inout)                     :: INFO
    integer(ik4)                                    :: i, j
    integer(ik4), allocatable, dimension(:)         :: IPIV
    real(rkdp), dimension(ROWS,ROWS), intent(inout) :: MATRIX
    real(rkdp), dimension(ROWS,ROWS), intent(inout) :: INVERSE_MATRIX
    character(len=40)                               :: NAME_ROUTINE
    
    INFO         = 0_ik4
    NAME_ROUTINE = 'DGESV'
    
    call ROUTINE_ALLOCATE_VECTOR_INTEGER(ROWS, IPIV)
    
    LOOP_OUTER: do i=1_ik4,ROWS
      LOOP_INNER: do j=1_ik4,ROWS
        if (i .EQ. j) then
          INVERSE_MATRIX(i,j) = 1.0_rkdp
        else
          INVERSE_MATRIX(i,j) = 0.0_rkdp
        end if
      end do LOOP_INNER
    end do LOOP_OUTER
    
    call DGESV(ROWS, ROWS, MATRIX, ROWS, IPIV, INVERSE_MATRIX, ROWS, INFO)
    call ROUTINE_ERROR_MESSAGE_INFO_LAPACK(INFO, NAME_ROUTINE)
    
    call ROUTINE_DEALLOCATE_VECTOR_INTEGER(IPIV)
  end subroutine ROUTINE_MATRIX_INVERSION_DGESV
  
  
!**********************************************************
!**     Subroutine to invert a matrix with the DSYTRI2X  **
!**     solver of LAPACK:                                **
!**                                                      **
!**     Structure of DSYTRI2X:                           **
!**       - DSYTRI2X(UPLO, N, A, LDA, IPIV, WORK, NB,    **
!**                 INFO)                                **
!**                                                      **
!**     Used variables:                                  **
!**       - UPLO  --> 'L'                                **
!**       - N     --> ROWS                               **
!**       - A     --> INVERSE_MATRIX                     **
!**       - LDA   --> ROWS                               **
!**       - IPIV  --> IPIV                               **
!**       - WORK  --> WORKM                              **
!**       - NB    --> 64                                 **
!**       - INFO  --> INFO                               **
!**********************************************************
  subroutine ROUTINE_MATRIX_INVERSION_DSYTRI2X(ROWS, MATRIX, INVERSE_MATRIX, INFO)
    implicit none
    
    integer(ik4), intent(in)                        :: ROWS
    integer(ik4), intent(inout)                     :: INFO
    integer(ik4)                                    :: i, j, NB, NNB
    integer, external                               :: ilaenv
    integer(ik4), allocatable, dimension(:)         :: IPIV
    real(rkdp), allocatable, dimension(:)           :: WORKV
    real(rkdp), allocatable, dimension(:,:)         :: WORKM
    real(rkdp), dimension(ROWS,ROWS), intent(in)    :: MATRIX
    real(rkdp), dimension(ROWS,ROWS), intent(inout) :: INVERSE_MATRIX
    character(len=40)                               :: NAME_ROUTINE
    
    INFO         = 0_ik4
    NB           = ilaenv(1_ik4, 'DSYTRF', 'L', ROWS, -1_ik4, -1_ik4, -1_ik4)
    NAME_ROUTINE = 'DSYTRF'
    
    !call ROUTINE_PRINT_INTEGER(NB) !<--
    call ROUTINE_ALLOCATE_VECTOR_INTEGER(ROWS, IPIV)
    call ROUTINE_ALLOCATE_VECTOR_REAL(ROWS*NB, WORKV)
    call ROUTINE_ALLOCATE_MATRIX_REAL(ROWS+NB+1_ik4, NB+3_ik4, WORKM)
    
    LOOP_OUTER_1: do i=1_ik4,ROWS
      LOOP_INNER_1: do j=1_ik4,ROWS
        INVERSE_MATRIX(i,j) = MATRIX(i,j)
      end do LOOP_INNER_1
    end do LOOP_OUTER_1
    
    NNB = ROWS*NB
    call DSYTRF('L', ROWS, INVERSE_MATRIX, ROWS, IPIV, WORKV, NNB, INFO)
    call ROUTINE_ERROR_MESSAGE_INFO_LAPACK(INFO, NAME_ROUTINE)
    
    !call ROUTINE_PRINT_REAL(WORKV(1_ik4)) !<--
    NAME_ROUTINE = 'DSYTRI2X'
    call DSYTRI2X('L', ROWS, INVERSE_MATRIX, ROWS, IPIV, WORKM, NB, INFO)
    call ROUTINE_ERROR_MESSAGE_INFO_LAPACK(INFO, NAME_ROUTINE)
    
    LOOP_OUTER_2: do i=1_ik4,ROWS
     LOOP_INNER_2: do j=i+1_ik4,ROWS
        INVERSE_MATRIX(i,j) = INVERSE_MATRIX(j,i)
      end do LOOP_INNER_2
    end do LOOP_OUTER_2
    
    call ROUTINE_DEALLOCATE_VECTOR_INTEGER(IPIV)
    call ROUTINE_DEALLOCATE_VECTOR_REAL(WORKV)
    call ROUTINE_DEALLOCATE_MATRIX_REAL(WORKM)
  end subroutine ROUTINE_MATRIX_INVERSION_DSYTRI2X
  
  
!**********************************************************
!**     Subroutine to calculate "a*A*B+b*C" with the     **
!**     DGEMM solver of BLAS:                            **
!**                                                      **
!**     Structure of DGEMM:                              **
!**       - DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A,     **
!**               LDA, B, LDB, BETA, C, LDC)             **
!**                                                      **
!**     Used variables:                                  **
!**       - TRANSA --> 'N'                               **
!**       - TRANSB --> 'N'                               **
!**       - M      --> ROWS_PRODUCT                      **
!**       - N      --> COLUMNS_PRODUCT                   **
!**       - K      --> COLUMNS_LEFT                      **
!**       - ALPHA  --> 1.0                               **
!**       - A      --> MATRIX_LEFT                       **
!**       - LDA    --> ROWS_PRODUCT                      **
!**       - B      --> MATRIX_RIGHT                      **
!**       - LDB    --> COLUMNS_LEFT                      **
!**       - BETA   --> 0.0                               **
!**       - C      --> MATRIX_PRODUCT                    **
!**       - LDC    --> ROWS_PRODUCT                      **
!**********************************************************
  subroutine ROUTINE_MATRIX_MATRIX_MULT_DGEMM(ROWS_PRODUCT, COLUMNS_LEFT, COLUMNS_PRODUCT, &
             & MATRIX_LEFT, MATRIX_RIGHT, MATRIX_PRODUCT)
    implicit none
    
    integer(ik4), intent(in)                                           :: ROWS_PRODUCT, COLUMNS_LEFT, COLUMNS_PRODUCT
    real(rkdp), dimension(ROWS_PRODUCT,COLUMNS_LEFT), intent(in)       :: MATRIX_LEFT
    real(rkdp), dimension(COLUMNS_LEFT,COLUMNS_PRODUCT), intent(in)    :: MATRIX_RIGHT
    real(rkdp), dimension(ROWS_PRODUCT,COLUMNS_PRODUCT), intent(inout) :: MATRIX_PRODUCT
    
    call DGEMM('N', 'N', ROWS_PRODUCT, COLUMNS_PRODUCT, COLUMNS_LEFT, 1.0_rkdp, MATRIX_LEFT, ROWS_PRODUCT, &
         & MATRIX_RIGHT, COLUMNS_LEFT, 0.0_rkdp, MATRIX_PRODUCT, ROWS_PRODUCT)
  end subroutine ROUTINE_MATRIX_MATRIX_MULT_DGEMM
  
  
!**********************************************************
!**     Subroutine to calculate "a*A'*B+b*C" with the    **
!**     DGEMM solver of BLAS:                            **
!**                                                      **
!**     Structure of DGEMM:                              **
!**       - DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A,     **
!**               LDA, B, LDB, BETA, C, LDC)             **
!**                                                      **
!**     Used variables:                                  **
!**       - TRANSA --> 'T'                               **
!**       - TRANSB --> 'N'                               **
!**       - M      --> ROWS_PRODUCT                      **
!**       - N      --> COLUMNS_PRODUCT                   **
!**       - K      --> COLUMNS_LEFT                      **
!**       - ALPHA  --> 1.0                               **
!**       - A      --> MATRIX_LEFT                       **
!**       - LDA    --> COLUMNS_LEFT                      **
!**       - B      --> MATRIX_RIGHT                      **
!**       - LDB    --> COLUMNS_LEFT                      **
!**       - BETA   --> 0.0                               **
!**       - C      --> MATRIX_PRODUCT                    **
!**       - LDC    --> ROWS_PRODUCT                      **
!**********************************************************
  subroutine ROUTINE_MATRIX_TRANSP_MATRIX_MULT_DGEMM(ROWS_PRODUCT, COLUMNS_LEFT, COLUMNS_PRODUCT, &
             & MATRIX_LEFT, MATRIX_RIGHT, MATRIX_PRODUCT)
    implicit none
    
    integer(ik4), intent(in)                                           :: ROWS_PRODUCT, COLUMNS_LEFT, COLUMNS_PRODUCT
    real(rkdp), dimension(COLUMNS_LEFT,ROWS_PRODUCT), intent(in)       :: MATRIX_LEFT
    real(rkdp), dimension(COLUMNS_LEFT,COLUMNS_PRODUCT), intent(in)    :: MATRIX_RIGHT
    real(rkdp), dimension(ROWS_PRODUCT,COLUMNS_PRODUCT), intent(inout) :: MATRIX_PRODUCT
    
    call DGEMM('T', 'N', ROWS_PRODUCT, COLUMNS_PRODUCT, COLUMNS_LEFT, 1.0_rkdp, MATRIX_LEFT, COLUMNS_LEFT, &
         & MATRIX_RIGHT, COLUMNS_LEFT, 0.0_rkdp, MATRIX_PRODUCT, ROWS_PRODUCT)
  end subroutine ROUTINE_MATRIX_TRANSP_MATRIX_MULT_DGEMM
  
  
!**********************************************************
!**     Subroutine to calculate "a*A*x+b*y" with the     **
!**     DGEMV solver of BLAS:                            **
!**                                                      **
!**     Structure of DGEMV:                              **
!**       - DGEMV(TRANS, M, N, ALPHA, A, LDA, X, INCX,   **
!**               BETA, Y, INCY)                         **
!**                                                      **
!**     Used variables:                                  **
!**       - TRANS --> 'N'                                **
!**       - M     --> ROWS                               **
!**       - N     --> COLUMNS                            **
!**       - ALPHA --> 1.0                                **
!**       - A     --> MATRIX                             **
!**       - LDA   --> ROWS                               **
!**       - X     --> VECTOR                             **
!**       - INCX  --> 1                                  **
!**       - BETA  --> 0.0                                **
!**       - Y     --> VECTOR_PRODUCT                     **
!**       - INCY  --> 1                                  **
!**********************************************************
  subroutine ROUTINE_MATRIX_VECTOR_MULT_DGEMV(ROWS, COLUMNS, MATRIX, VECTOR, VECTOR_PRODUCT)
    implicit none
    
    integer(ik4), intent(in)                         :: ROWS, COLUMNS
    real(rkdp), dimension(COLUMNS), intent(in)       :: VECTOR
    real(rkdp), dimension(ROWS), intent(inout)       :: VECTOR_PRODUCT
    real(rkdp), dimension(ROWS, COLUMNS), intent(in) :: MATRIX
    
    call DGEMV('N', ROWS, COLUMNS, 1.0_rkdp, MATRIX, ROWS, VECTOR, 1_ik4, 0.0_rkdp, VECTOR_PRODUCT, 1_ik4)
  end subroutine ROUTINE_MATRIX_VECTOR_MULT_DGEMV
  
  
!**********************************************************
!**     Subroutine to calculate "a*A'*x+b*y" with the    **
!**     DGEMV solver of BLAS:                            **
!**                                                      **
!**     Structure of DGEMV:                              **
!**       - DGEMV(TRANS, M, N, ALPHA, A, LDA, X, INCX,   **
!**               BETA, Y, INCY)                         **
!**                                                      **
!**     Used variables:                                  **
!**       - TRANS --> 'T'                                **
!**       - M     --> ROWS                               **
!**       - N     --> COLUMNS                            **
!**       - ALPHA --> 1.0                                **
!**       - A     --> MATRIX                             **
!**       - LDA   --> ROWS                               **
!**       - X     --> VECTOR                             **
!**       - INCX  --> 1                                  **
!**       - BETA  --> 0.0                                **
!**       - Y     --> VECTOR_PRODUCT                     **
!**       - INCY  --> 1                                  **
!**********************************************************
  subroutine ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV(ROWS, COLUMNS, MATRIX, VECTOR, VECTOR_PRODUCT)
    implicit none
    
    integer(ik4), intent(in)                         :: ROWS, COLUMNS
    real(rkdp), dimension(ROWS), intent(in)          :: VECTOR
    real(rkdp), dimension(COLUMNS), intent(inout)    :: VECTOR_PRODUCT
    real(rkdp), dimension(ROWS, COLUMNS), intent(in) :: MATRIX
    
    call DGEMV('T', ROWS, COLUMNS, 1.0_rkdp, MATRIX, ROWS, VECTOR, 1_ik4, 0.0_rkdp, VECTOR_PRODUCT, 1_ik4)
  end subroutine ROUTINE_MATRIX_TRANSP_VECTOR_MULT_DGEMV
  
  
!**********************************************************
!**     Subroutine to center the columns of a matrix:    **
!**********************************************************
  subroutine ROUTINE_CENTER_COLUMNS(ROWS, COLUMNS, MATRIX)
    implicit none
    
    integer(ik4), intent(in)                           :: ROWS, COLUMNS
    integer(ik4)                                       :: i, j
    real(rkdp)                                         :: COLUMN_SUM
    real(rkdp), dimension(ROWS,COLUMNS), intent(inout) :: MATRIX
    
    LOOP_COLUMNS: do j=1_ik4,COLUMNS
      COLUMN_SUM = 0.0_rkdp
      
      LOOP_ROWS_1: do i=1_ik4,ROWS
        COLUMN_SUM = COLUMN_SUM+MATRIX(i,j)
      end do LOOP_ROWS_1
      
      COLUMN_SUM = COLUMN_SUM/real(ROWS,rkdp)
      
      LOOP_ROWS_2: do i=1_ik4,ROWS
        MATRIX(i,j) = MATRIX(i,j)-COLUMN_SUM
      end do LOOP_ROWS_2
    end do LOOP_COLUMNS
  end subroutine ROUTINE_CENTER_COLUMNS
  
  
!**********************************************************
!**     Subroutine to calculate the means of the col-    **
!**     umns of a matrix. The means will be written in   **
!**     an output file:                                  **
!**********************************************************
  subroutine ROUTINE_CALCULATE_MEANS(ROWS, COLUMNS, MATRIX)
    implicit none
    
    integer(ik4), intent(in)                        :: ROWS, COLUMNS
    integer(ik4)                                    :: i, j, io_error
    real(rkdp)                                      :: COLUMN_SUM
    real(rkdp), dimension(ROWS,COLUMNS), intent(in) :: MATRIX
    character(len=40)                               :: PLACE_ERROR
    character(len=200)                              :: FILE_NAME
    
    FILE_NAME   = 'Means.txt'
    PLACE_ERROR = 'ROUTINE CALCULATE MEANS'
    
    open(unit=21, file=FILE_NAME, status='unknown', action='write', iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_OPEN(FILE_NAME, PLACE_ERROR, io_error)
    
    LOOP_COLUMNS: do j=1,COLUMNS
      COLUMN_SUM = 0.0_rkdp
      
      LOOP_ROWS_1: do i=1,ROWS
        COLUMN_SUM = COLUMN_SUM+MATRIX(i,j)
      end do LOOP_ROWS_1
      
      COLUMN_SUM = COLUMN_SUM/real(ROWS,rkdp)
      
      write(21, '(1x,F20.12)', iostat=io_error) COLUMN_SUM
      call ROUTINE_ERROR_MESSAGE_WRITE(FILE_NAME, PLACE_ERROR, io_error)
    end do LOOP_COLUMNS
    
    close(21, iostat=io_error)
    call ROUTINE_ERROR_MESSAGE_CLOSE(FILE_NAME, PLACE_ERROR, io_error)
  end subroutine ROUTINE_CALCULATE_MEANS
end module MODULE_LVL4_MATRIX_OPERATIONS
