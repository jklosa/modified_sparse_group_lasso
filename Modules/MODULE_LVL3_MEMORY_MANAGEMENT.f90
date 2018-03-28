!**********************************************************
!**********************************************************
!**                                                      **
!**     ---- MODULE_LVL3_MEMORY_MANAGEMENT.f90 ----      **
!**                                                      **
!**     This module contains the following subroutines   **
!**     for memory allocation, initialization and de-    **
!**     allocation:                                      **
!**       - ROUTINE_ALLOCATE_VECTOR_INTEGER              **
!**       - ROUTINE_ALLOCATE_VECTOR_REAL                 **
!**       - ROUTINE_ALLOCATE_VECTOR_LOGICAL_TRUE         **
!**       - ROUTINE_ALLOCATE_VECTOR_LOGICAL_FALSE        **
!**       - ROUTINE_ALLOCATE_VECTOR_CHARACTER_200        **
!**       - ROUTINE_ALLOCATE_MATRIX_INTEGER              **
!**       - ROUTINE_ALLOCATE_MATRIX_REAL                 **
!**       - ROUTINE_ALLOCATE_MATRIX_LOGICAL_TRUE         **
!**       - ROUTINE_ALLOCATE_MATRIX_LOGICAL_FALSE        **
!**       - ROUTINE_ALLOCATE_MATRIX_CHARACTER_200        **
!**       - ROUTINE_ALLOCATE_3D_ARRAY_INTEGER            **
!**       - ROUTINE_ALLOCATE_3D_ARRAY_REAL               **
!**       - ROUTINE_ALLOCATE_3D_ARRAY_LOGICAL_TRUE       **
!**       - ROUTINE_ALLOCATE_3D_ARRAY_LOGICAL_FALSE      **
!**       - ROUTINE_ALLOCATE_3D_ARRAY_CHARACTER_200      **
!**       - ROUTINE_DEALLOCATE_VECTOR_INTEGER            **
!**       - ROUTINE_DEALLOCATE_VECTOR_REAL               **
!**       - ROUTINE_DEALLOCATE_VECTOR_LOGICAL            **
!**       - ROUTINE_DEALLOCATE_VECTOR_CHARACTER_200      **
!**       - ROUTINE_DEALLOCATE_MATRIX_INTEGER            **
!**       - ROUTINE_DEALLOCATE_MATRIX_REAL               **
!**       - ROUTINE_DEALLOCATE_MATRIX_LOGICAL            **
!**       - ROUTINE_DEALLOCATE_MATRIX_CHARACTER_200      **
!**       - ROUTINE_DEALLOCATE_3D_ARRAY_INTEGER          **
!**       - ROUTINE_DEALLOCATE_3D_ARRAY_REAL             **
!**       - ROUTINE_DEALLOCATE_3D_ARRAY_LOGICAL          **
!**       - ROUTINE_DEALLOCATE_3D_ARRAY_CHARACTER_200    **
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

module MODULE_LVL3_MEMORY_MANAGEMENT
  use MODULE_LVL1_KIND_NUMBERS
  use MODULE_LVL2_ON_SCREEN_MESSAGES
  implicit none
  
  contains
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of an integer vector:                     **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_VECTOR_INTEGER(LENGTH, VECTOR)
    implicit none
    
    integer(ik4), intent(in)                               :: LENGTH
    integer(ik4)                                           :: i, alloc_error
    integer(ik4), allocatable, dimension(:), intent(inout) :: VECTOR
    character(len=40)                                      :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'VECTOR'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_VECTOR_INTEGER'
    
    allocate(VECTOR(LENGTH), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL: do i=1_ik4,LENGTH
      VECTOR(i) = 0_ik4
    end do LOOP_FILL
  end subroutine ROUTINE_ALLOCATE_VECTOR_INTEGER
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a real-valued vector:                  **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_VECTOR_REAL(LENGTH, VECTOR)
    implicit none
    
    integer(ik4), intent(in)                             :: LENGTH
    integer(ik4)                                         :: i, alloc_error
    real(rkdp), allocatable, dimension(:), intent(inout) :: VECTOR
    character(len=40)                                    :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'VECTOR'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_VECTOR_REAL'
    
    allocate(VECTOR(LENGTH), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL: do i=1_ik4,LENGTH
      VECTOR(i) = 0.0_rkdp
    end do LOOP_FILL
  end subroutine ROUTINE_ALLOCATE_VECTOR_REAL
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a logical(.TRUE.) vector:              **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_VECTOR_LOGICAL_TRUE(LENGTH, VECTOR)
    implicit none
    
    integer(ik4), intent(in)                          :: LENGTH
    integer(ik4)                                      :: i, alloc_error
    logical, allocatable, dimension(:), intent(inout) :: VECTOR
    character(len=40)                                 :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'VECTOR'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_VECTOR_LOGICAL_TRUE'
    
    allocate(VECTOR(LENGTH), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL: do i=1_ik4,LENGTH
      VECTOR(i) = .TRUE.
    end do LOOP_FILL
  end subroutine ROUTINE_ALLOCATE_VECTOR_LOGICAL_TRUE
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a logical(.FALSE.) vector:             **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_VECTOR_LOGICAL_FALSE(LENGTH, VECTOR)
    implicit none
    
    integer(ik4), intent(in)                          :: LENGTH
    integer(ik4)                                      :: i, alloc_error
    logical, allocatable, dimension(:), intent(inout) :: VECTOR
    character(len=40)                                 :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'VECTOR'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_VECTOR_LOGICAL_FALSE'
    
    allocate(VECTOR(LENGTH), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL: do i=1_ik4,LENGTH
      VECTOR(i) = .FALSE.
    end do LOOP_FILL
  end subroutine ROUTINE_ALLOCATE_VECTOR_LOGICAL_FALSE
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a character(len=200) vector:           **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_VECTOR_CHARACTER_200(LENGTH, VECTOR)
    implicit none
    
    integer(ik4), intent(in)                                     :: LENGTH
    integer(ik4)                                                 :: i, alloc_error
    character(len=40)                                            :: NAME_VARIABLE, PLACE_ERROR
    character(len=200), allocatable, dimension(:), intent(inout) :: VECTOR
    
    NAME_VARIABLE = 'VECTOR'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_VECTOR_CHARACTER_200'
    
    allocate(VECTOR(LENGTH), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL: do i=1_ik4,LENGTH
      VECTOR(i) = ''
    end do LOOP_FILL
  end subroutine ROUTINE_ALLOCATE_VECTOR_CHARACTER_200
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of an integer matrix:                     **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_MATRIX_INTEGER(ROWS, COLUMNS, MATRIX)
    implicit none
    
    integer(ik4), intent(in)                                 :: ROWS, COLUMNS
    integer(ik4)                                             :: i, j, alloc_error
    integer(ik4), allocatable, dimension(:,:), intent(inout) :: MATRIX
    character(len=40)                                        :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'MATRIX'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_MATRIX_INTEGER'
    
    allocate(MATRIX(ROWS,COLUMNS), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL_ROWS: do i=1_ik4,ROWS
      LOOP_FILL_COLUMNS: do j=1_ik4,COLUMNS
        MATRIX(i,j) = 0_ik4
      end do LOOP_FILL_COLUMNS
    end do LOOP_FILL_ROWS
  end subroutine ROUTINE_ALLOCATE_MATRIX_INTEGER
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a real-valued matrix:                  **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_MATRIX_REAL(ROWS, COLUMNS, MATRIX)
    implicit none
    
    integer(ik4), intent(in)                               :: ROWS, COLUMNS
    integer(ik4)                                           :: i, j, alloc_error
    real(rkdp), allocatable, dimension(:,:), intent(inout) :: MATRIX
    character(len=40)                                      :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'MATRIX'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_MATRIX_REAL'
    
    allocate(MATRIX(ROWS,COLUMNS), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL_ROWS: do i=1_ik4,ROWS
      LOOP_FILL_COLUMNS: do j=1_ik4,COLUMNS
        MATRIX(i,j) = 0.0_rkdp
      end do LOOP_FILL_COLUMNS
    end do LOOP_FILL_ROWS
  end subroutine ROUTINE_ALLOCATE_MATRIX_REAL
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a logical(.TRUE.) matrix:              **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_MATRIX_LOGICAL_TRUE(ROWS, COLUMNS, MATRIX)
    implicit none
    
    integer(ik4), intent(in)                            :: ROWS, COLUMNS
    integer(ik4)                                        :: i, j, alloc_error
    logical, allocatable, dimension(:,:), intent(inout) :: MATRIX
    character(len=40)                                   :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'MATRIX'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_MATRIX_LOGICAL_TRUE'
    
    allocate(MATRIX(ROWS,COLUMNS), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL_ROWS: do i=1_ik4,ROWS
      LOOP_FILL_COLUMNS: do j=1_ik4,COLUMNS
        MATRIX(i,j) = .TRUE.
      end do LOOP_FILL_COLUMNS
    end do LOOP_FILL_ROWS
  end subroutine ROUTINE_ALLOCATE_MATRIX_LOGICAL_TRUE
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a logical(.FALSE.) matrix:             **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_MATRIX_LOGICAL_FALSE(ROWS, COLUMNS, MATRIX)
    implicit none
    
    integer(ik4), intent(in)                            :: ROWS, COLUMNS
    integer(ik4)                                        :: i, j, alloc_error
    logical, allocatable, dimension(:,:), intent(inout) :: MATRIX
    character(len=40)                                   :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'MATRIX'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_MATRIX_LOGICAL_FALSE'
    
    allocate(MATRIX(ROWS,COLUMNS), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL_ROWS: do i=1_ik4,ROWS
      LOOP_FILL_COLUMNS: do j=1_ik4,COLUMNS
        MATRIX(i,j) = .FALSE.
      end do LOOP_FILL_COLUMNS
    end do LOOP_FILL_ROWS
  end subroutine ROUTINE_ALLOCATE_MATRIX_LOGICAL_FALSE
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a character(len=200) matrix:           **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_MATRIX_CHARACTER_200(ROWS, COLUMNS, MATRIX)
    implicit none
    
    integer(ik4), intent(in)                                       :: ROWS, COLUMNS
    integer(ik4)                                                   :: i, j, alloc_error
    character(len=40)                                              :: NAME_VARIABLE, PLACE_ERROR
    character(len=200), allocatable, dimension(:,:), intent(inout) :: MATRIX
    
    NAME_VARIABLE = 'MATRIX'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_MATRIX_CHARACTER_200'
    
    allocate(MATRIX(ROWS,COLUMNS), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL_ROWS: do i=1_ik4,ROWS
      LOOP_FILL_COLUMNS: do j=1_ik4,COLUMNS
        MATRIX(i,j) = ''
      end do LOOP_FILL_COLUMNS
    end do LOOP_FILL_ROWS
  end subroutine ROUTINE_ALLOCATE_MATRIX_CHARACTER_200
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of an integer 3D array:                   **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_3D_ARRAY_INTEGER(ROWS, COLUMNS, DEPTH, ARRAY_3D)
    implicit none
    
    integer(ik4), intent(in)                                   :: ROWS, COLUMNS, DEPTH
    integer(ik4)                                               :: i, j, k, alloc_error
    integer(ik4), allocatable, dimension(:,:,:), intent(inout) :: ARRAY_3D
    character(len=40)                                          :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'ARRAY_3D'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_3D_ARRAY_INTEGER'
    
    allocate(ARRAY_3D(ROWS,COLUMNS,DEPTH), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL_ROWS: do i=1_ik4,ROWS
      LOOP_FILL_COLUMNS: do j=1_ik4,COLUMNS
        LOOP_FILL_DEPTH: do k=1_ik4,DEPTH
          ARRAY_3D(i,j,k) = 0_ik4
        end do LOOP_FILL_DEPTH
      end do LOOP_FILL_COLUMNS
    end do LOOP_FILL_ROWS
  end subroutine ROUTINE_ALLOCATE_3D_ARRAY_INTEGER
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a real-valued 3D array:                **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_3D_ARRAY_REAL(ROWS, COLUMNS, DEPTH, ARRAY_3D)
    implicit none
    
    integer(ik4), intent(in)                                 :: ROWS, COLUMNS, DEPTH
    integer(ik4)                                             :: i, j, k, alloc_error
    real(rkdp), allocatable, dimension(:,:,:), intent(inout) :: ARRAY_3D
    character(len=40)                                        :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'ARRAY_3D'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_3D_ARRAY_REAL'
    
    allocate(ARRAY_3D(ROWS,COLUMNS,DEPTH), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL_ROWS: do i=1_ik4,ROWS
      LOOP_FILL_COLUMNS: do j=1_ik4,COLUMNS
        LOOP_FILL_DEPTH: do k=1_ik4,DEPTH
          ARRAY_3D(i,j,k) = 0.0_rkdp
        end do LOOP_FILL_DEPTH
      end do LOOP_FILL_COLUMNS
    end do LOOP_FILL_ROWS
  end subroutine ROUTINE_ALLOCATE_3D_ARRAY_REAL
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a logical(.TRUE.) 3D array:            **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_3D_ARRAY_LOGICAL_TRUE(ROWS, COLUMNS, DEPTH, ARRAY_3D)
    implicit none
    
    integer(ik4), intent(in)                              :: ROWS, COLUMNS, DEPTH
    integer(ik4)                                          :: i, j, k, alloc_error
    logical, allocatable, dimension(:,:,:), intent(inout) :: ARRAY_3D
    character(len=40)                                     :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'ARRAY_3D'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_3D_ARRAY_LOGICAL_TRUE'
    
    allocate(ARRAY_3D(ROWS,COLUMNS,DEPTH), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL_ROWS: do i=1_ik4,ROWS
      LOOP_FILL_COLUMNS: do j=1_ik4,COLUMNS
        LOOP_FILL_DEPTH: do k=1_ik4,DEPTH
          ARRAY_3D(i,j,k) = .TRUE.
        end do LOOP_FILL_DEPTH
      end do LOOP_FILL_COLUMNS
    end do LOOP_FILL_ROWS
  end subroutine ROUTINE_ALLOCATE_3D_ARRAY_LOGICAL_TRUE
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a logical(.FALSE.) 3D array:           **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_3D_ARRAY_LOGICAL_FALSE(ROWS, COLUMNS, DEPTH, ARRAY_3D)
    implicit none
    
    integer(ik4), intent(in)                              :: ROWS, COLUMNS, DEPTH
    integer(ik4)                                          :: i, j, k, alloc_error
    logical, allocatable, dimension(:,:,:), intent(inout) :: ARRAY_3D
    character(len=40)                                     :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'ARRAY_3D'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_3D_ARRAY_LOGICAL_FALSE'
    
    allocate(ARRAY_3D(ROWS,COLUMNS,DEPTH), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL_ROWS: do i=1_ik4,ROWS
      LOOP_FILL_COLUMNS: do j=1_ik4,COLUMNS
        LOOP_FILL_DEPTH: do k=1_ik4,DEPTH
          ARRAY_3D(i,j,k) = .FALSE.
        end do LOOP_FILL_DEPTH
      end do LOOP_FILL_COLUMNS
    end do LOOP_FILL_ROWS
  end subroutine ROUTINE_ALLOCATE_3D_ARRAY_LOGICAL_FALSE
  
  
!**********************************************************
!**     Subroutine for memory allocation and initiali-   **
!**     zation of a character(len=200) 3D array:         **
!**********************************************************
  subroutine ROUTINE_ALLOCATE_3D_ARRAY_CHARACTER_200(ROWS, COLUMNS, DEPTH, ARRAY_3D)
    implicit none
    
    integer(ik4), intent(in)                                         :: ROWS, COLUMNS, DEPTH
    integer(ik4)                                                     :: i, j, k, alloc_error
    character(len=40)                                                :: NAME_VARIABLE, PLACE_ERROR
    character(len=200), allocatable, dimension(:,:,:), intent(inout) :: ARRAY_3D
    
    NAME_VARIABLE = 'ARRAY_3D'
    PLACE_ERROR   = 'ROUTINE_ALLOCATE_3D_ARRAY_CHARACTER_200'
    
    allocate(ARRAY_3D(ROWS,COLUMNS,DEPTH), stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_ALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
    
    LOOP_FILL_ROWS: do i=1_ik4,ROWS
      LOOP_FILL_COLUMNS: do j=1_ik4,COLUMNS
        LOOP_FILL_DEPTH: do k=1_ik4,DEPTH
          ARRAY_3D(i,j,k) = ''
        end do LOOP_FILL_DEPTH
      end do LOOP_FILL_COLUMNS
    end do LOOP_FILL_ROWS
  end subroutine ROUTINE_ALLOCATE_3D_ARRAY_CHARACTER_200
  
  
!**********************************************************
!**     Subroutine for memory deallocation of an inte-   **
!**     ger vector:                                      **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_VECTOR_INTEGER(VECTOR)
    implicit none
    
    integer(ik4)                                           :: alloc_error
    integer(ik4), allocatable, dimension(:), intent(inout) :: VECTOR
    character(len=40)                                      :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'VECTOR'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_VECTOR_INTEGER'
    
    deallocate(VECTOR, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_VECTOR_INTEGER
  
  
!**********************************************************
!**     Subroutine for memory deallocation of a real-    **
!**     valued vector:                                   **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_VECTOR_REAL(VECTOR)
    implicit none
    
    integer(ik4)                                         :: alloc_error
    real(rkdp), allocatable, dimension(:), intent(inout) :: VECTOR
    character(len=40)                                    :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'VECTOR'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_VECTOR_REAL'
    
    deallocate(VECTOR, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_VECTOR_REAL
  
  
!**********************************************************
!**     Subroutine for memory deallocation of a logi-    **
!**     cal vector:                                      **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_VECTOR_LOGICAL(VECTOR)
    implicit none
    
    integer(ik4)                                      :: alloc_error
    logical, allocatable, dimension(:), intent(inout) :: VECTOR
    character(len=40)                                 :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'VECTOR'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_VECTOR_LOGICAL'
    
    deallocate(VECTOR, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_VECTOR_LOGICAL
  
  
!**********************************************************
!**     Subroutine for memory deallocation of a char-    **
!**     acter(len=200) vector:                           **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_VECTOR_CHARACTER_200(VECTOR)
    implicit none
    
    integer(ik4)                                                 :: alloc_error
    character(len=40)                                            :: NAME_VARIABLE, PLACE_ERROR
    character(len=200), allocatable, dimension(:), intent(inout) :: VECTOR
    
    NAME_VARIABLE = 'VECTOR'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_VECTOR_CHARACTER_200'
    
    deallocate(VECTOR, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_VECTOR_CHARACTER_200
  
  
!**********************************************************
!**     Subroutine for memory deallocation of an inte-   **
!**     ger matrix:                                      **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_MATRIX_INTEGER(MATRIX)
    implicit none
    
    integer(ik4)                                             :: alloc_error
    integer(ik4), allocatable, dimension(:,:), intent(inout) :: MATRIX
    character(len=40)                                        :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'MATRIX'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_MATRIX_INTEGER'
    
    deallocate(MATRIX, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_MATRIX_INTEGER
  
  
!**********************************************************
!**     Subroutine for memory deallocation of a real-    **
!**     valued matrix:                                   **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_MATRIX_REAL(MATRIX)
    implicit none
    
    integer(ik4)                                           :: alloc_error
    real(rkdp), allocatable, dimension(:,:), intent(inout) :: MATRIX
    character(len=40)                                      :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'MATRIX'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_MATRIX_REAL'
    
    deallocate(MATRIX, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_MATRIX_REAL
  
  
!**********************************************************
!**     Subroutine for memory deallocation of a logi-    **
!**     cal matrix:                                      **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_MATRIX_LOGICAL(MATRIX)
    implicit none
    
    integer(ik4)                                        :: alloc_error
    logical, allocatable, dimension(:,:), intent(inout) :: MATRIX
    character(len=40)                                   :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'MATRIX'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_MATRIX_LOGICAL'
    
    deallocate(MATRIX, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_MATRIX_LOGICAL
  
  
!**********************************************************
!**     Subroutine for memory deallocation of a char-    **
!**     acter(len=200) matrix:                           **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_MATRIX_CHARACTER_200(MATRIX)
    implicit none
    
    integer(ik4)                                                   :: alloc_error
    character(len=40)                                              :: NAME_VARIABLE, PLACE_ERROR
    character(len=200), allocatable, dimension(:,:), intent(inout) :: MATRIX
    
    NAME_VARIABLE = 'MATRIX'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_MATRIX_CHARACTER_200'
    
    deallocate(MATRIX, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_MATRIX_CHARACTER_200
  
  
!**********************************************************
!**     Subroutine for memory deallocation of an inte-   **
!**     ger 3D array:                                    **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_3D_ARRAY_INTEGER(ARRAY_3D)
    implicit none
    
    integer(ik4)                                               :: alloc_error
    integer(ik4), allocatable, dimension(:,:,:), intent(inout) :: ARRAY_3D
    character(len=40)                                          :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'ARRAY_3D'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_3D_ARRAY_INTEGER'
    
    deallocate(ARRAY_3D, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_3D_ARRAY_INTEGER
  
  
!**********************************************************
!**     Subroutine for memory deallocation of a real-    **
!**     valued 3D array:                                 **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_3D_ARRAY_REAL(ARRAY_3D)
    implicit none
    
    integer(ik4)                                             :: alloc_error
    real(rkdp), allocatable, dimension(:,:,:), intent(inout) :: ARRAY_3D
    character(len=40)                                        :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'ARRAY_3D'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_3D_ARRAY_REAL'
    
    deallocate(ARRAY_3D, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_3D_ARRAY_REAL
  
  
!**********************************************************
!**     Subroutine for memory deallocation of a logi-    **
!**     cal 3D array:                                    **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_3D_ARRAY_LOGICAL(ARRAY_3D)
    implicit none
    
    integer(ik4)                                          :: alloc_error
    logical, allocatable, dimension(:,:,:), intent(inout) :: ARRAY_3D
    character(len=40)                                     :: NAME_VARIABLE, PLACE_ERROR
    
    NAME_VARIABLE = 'ARRAY_3D'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_3D_ARRAY_LOGICAL'
    
    deallocate(ARRAY_3D, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_3D_ARRAY_LOGICAL
  
  
!**********************************************************
!**     Subroutine for memory deallocation of a char-    **
!**     acter(len=200) 3D array:                         **
!**********************************************************
  subroutine ROUTINE_DEALLOCATE_3D_ARRAY_CHARACTER_200(ARRAY_3D)
    implicit none
    
    integer(ik4)                                                     :: alloc_error
    character(len=40)                                                :: NAME_VARIABLE, PLACE_ERROR
    character(len=200), allocatable, dimension(:,:,:), intent(inout) :: ARRAY_3D
    
    NAME_VARIABLE = 'ARRAY_3D'
    PLACE_ERROR   = 'ROUTINE_DEALLOCATE_3D_ARRAY_CHARACTER_200'
    
    deallocate(ARRAY_3D, stat=alloc_error)
    call ROUTINE_ERROR_MESSAGE_DEALLOCATION(NAME_VARIABLE, PLACE_ERROR, alloc_error)
  end subroutine ROUTINE_DEALLOCATE_3D_ARRAY_CHARACTER_200
end module MODULE_LVL3_MEMORY_MANAGEMENT
