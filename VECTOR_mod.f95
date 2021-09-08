
!----- CONSTRUCTS -----------------------------------------------------------------------------!
TYPE :: VECTOR_TEMPLATE
    TEMPLATE, DIMENSION(:), ALLOCATABLE :: vals_
    INTEGER                             :: size_
    INTEGER                             :: capacity_
END TYPE
!----- CONSTRUCTS -----------------------------------------------------------------------------!

CONTAINS
    !----- MODULE SUBROUTINES -----------------------------------------------------------------!
    SUBROUTINE ASSIGN_CONTENT( this, new_size_, content_ )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        TEMPLATE             , INTENT(IN)    :: content_
        INTEGER              , INTENT(IN)    :: new_size_
        !----- INTERNALS ----------------------------------------------------------------------!
        INTEGER                              :: alloc_stat
        INTEGER                              :: i
        !----- VARIABLES ----------------------------------------------------------------------!
        
        IF( ALLOCATED(this%vals_) ) THEN
            DEALLOCATE( this%vals_ )
        ENDIF
        
        ALLOCATE( this%vals_(new_size_), STAT = alloc_stat )
        
        IF( alloc_stat /= 0 ) THEN
            WRITE(*, "(A)") "ERROR: in subroutine ASSIGN_CONTENT: Reallocation of &
            &vector_template%vals_ failed!"
            RETURN
        ENDIF
        
        this%size_     = new_size_
        this%capacity_ = new_size_
        
        DO i = 1, new_size_
            this%vals_(i) = content_
        ENDDO
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE PUSH_BACK( this, val_ )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        TEMPLATE             , INTENT(IN)    :: val_
        !----- INTERNALS ----------------------------------------------------------------------!
        TEMPLATE, DIMENSION(:), ALLOCATABLE  :: vals_realloc
        INTEGER                              :: alloc_stat
        INTEGER                              :: i
        !----- VARIABLES ----------------------------------------------------------------------!
        
        ! check for reallocation event:
        IF( this%size_ >= this%capacity_ ) THEN
            this%capacity_ = this%capacity_ * 2
            
            ALLOCATE( vals_realloc(this%capacity_), STAT = alloc_stat )
            
            IF( alloc_stat /= 0 ) THEN
                WRITE(*, "(A)") "ERROR: in subroutine PUSH_BACK: Allocation of vals_realloc &
                &failed!"
                RETURN
            ENDIF
            
            ! copy all values from the vector:
            DO i = 1, this%size_
                vals_realloc(i) = this%vals_(i)
            ENDDO
            
            this%size_ = this%size_ + 1
            vals_realloc(this%size_) = val_
            
            DEALLOCATE( this%vals_ )
            ALLOCATE  ( this%vals_(this%capacity_), STAT = alloc_stat )
            
            IF( alloc_stat /= 0 ) THEN
                WRITE(*, "(A)") "ERROR: in subroutine PUSH_BACK: Reallocation of VECTOR failed!"
                DEALLOCATE( vals_realloc )
                RETURN
            ENDIF
            
            this%vals_ = vals_realloc
            DEALLOCATE( vals_realloc )
        ELSE
            this%size_ = this%size_ + 1
            this%vals_(this%size_) = val_
        ENDIF
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE POP_BACK( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        !----- VARIABLES ----------------------------------------------------------------------!
        
        IF(this%size_ <= 0) RETURN
        
        this%size_ = this%size_ - 1
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE INSERT( this, val_, at_ )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        TEMPLATE             , INTENT(IN)    :: val_
        INTEGER              , INTENT(IN)    :: at_
        !----- INTERNALS ----------------------------------------------------------------------!
        TEMPLATE, DIMENSION(:), ALLOCATABLE  :: vals_realloc
        TEMPLATE                             :: temp
        INTEGER                              :: alloc_stat
        !----- VARIABLES ----------------------------------------------------------------------!
        
        ! check for reallocation event:
        IF( this%size_ >= this%capacity_ ) THEN
            this%capacity_ = this%capacity * 2
            
            ALLOCATE( vals_realloc(this%capacity_), STAT = alloc_stat )
            
            IF( alloc_stat /= 0 ) THEN
                WRITE(*, "(A)") "ERROR: in subroutine INSERT: Allocation of vals_realloc &
                &failed!"
                RETURN
            ENDIF
            
            ! copy all values from the vector before the insertion position:
            DO i = 1, at_ - 1
                vals_realloc(i) = this%vals_(i)
            ENDDO
            
            vals_realloc(at_) = val_
            this%size_ = this%size_ + 1
            
            ! copy the rest of the values from the old vector:
            DO i = at_ + 1, this%size_
                vals_realloc(i) = this%vals_(i - 1)
            ENDDO
            
            DEALLOCATE( this%vals_ )
            ALLOCATE  ( this%vals_(this%capacity_), STAT = alloc_stat )
            
            IF( alloc_stat /= 0 ) THEN
                WRITE(*, "(A)") "ERROR: in subroutine INSERT: Reallocation of VECTOR failed!"
                DEALLOCATE( vals_realloc )
                RETURN
            ENDIF
            
            this%vals_ = vals_realloc
            DEALLOCATE( vals_realloc )
        ELSE
            DO WHILE( i >= at_ )
                this%vals_(i + 1) = this%vals_(i)
                i = i - 1
            ENDDO
            this%vals_(at_) = val_
        ENDIF
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE ERASE( this, at_ )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE SWAP( this, other )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE CLEAR( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE RESIZE( this, new_capacity )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END SUBROUTINE
    !..........................................................................................!
    LOGICAL FUNCTION EMPTY( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END FUNCTION
    !..........................................................................................!
    SUBROUTINE RESERVE( this, new_capacity )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE SHRINK_TO_FIT( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END SUBROUTINE
    !..........................................................................................!
    TEMPLATE FUNCTION FRONT( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END FUNCTION
    !..........................................................................................!
    TEMPLATE FUNCTION BACK( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END FUNCTION
    !..........................................................................................!
    TEMPLATE FUNCTION AT( this, at_ )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END FUNCTION
    !----- MODULE SUBROUTINES -----------------------------------------------------------------!