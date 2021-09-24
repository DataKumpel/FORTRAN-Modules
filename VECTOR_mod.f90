
!----- CONSTRUCTS -----------------------------------------------------------------------------!
TYPE :: VECTOR_TEMPLATE
    TEMPLATE, DIMENSION(:), ALLOCATABLE :: vals_
    INTEGER                             :: size_
    INTEGER                             :: capacity_
END TYPE
!----- CONSTRUCTS -----------------------------------------------------------------------------!

CONTAINS
    !----- MODULE SUBROUTINES -----------------------------------------------------------------!
    SUBROUTINE VEC_ASSIGN_CONTENT( this, new_size_, content_ )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        TEMPLATE             , INTENT(IN)    :: content_
        INTEGER              , INTENT(IN)    :: new_size_
        !----- INTERNALS ----------------------------------------------------------------------!
        INTEGER                              :: alloc_stat
        INTEGER                              :: i
        !----- VARIABLES ----------------------------------------------------------------------!
        
        IF( ALLOCATED( this%vals_ ) ) THEN
            DEALLOCATE( this%vals_ )
        ENDIF
        
        ALLOCATE( this%vals_(new_size_), STAT = alloc_stat )
        
        IF( alloc_stat /= 0 ) THEN
            WRITE( *, "(A)" ) "ERROR: in subroutine ASSIGN_CONTENT: Reallocation of &
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
    SUBROUTINE VEC_PUSH_BACK( this, val_ )
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
                WRITE( *, "(A)" ) "ERROR: in subroutine PUSH_BACK: Allocation of vals_realloc &
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
                WRITE( *, "(A)" ) "ERROR: in subroutine PUSH_BACK: Reallocation of VECTOR &
                &failed!"
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
    SUBROUTINE VEC_POP_BACK( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        !----- VARIABLES ----------------------------------------------------------------------!
        
        IF(this%size_ <= 0) RETURN
        
        this%size_ = this%size_ - 1
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE VEC_INSERT( this, val_, at_ )
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
        
        ! don't insert at negative numbers:
        IF( at_ < 1 ) THEN
            WRITE( *, "(A)"     ) "ERROR: in subroutine INSERT: Insertion index was < 1 !"
            WRITE( *, "(A, I0)" ) "Index given : ", at_
            RETURN
        ENDIF
        
        ! insertion at higher positions than the vectors size results in PUSH_BACK:
        IF( at_ > this%size_ ) THEN
            CALL VEC_PUSH_BACK( this, val_ )
            RETURN
        ENDIF
        
        ! check for reallocation event:
        IF( this%size_ >= this%capacity_ ) THEN
            this%capacity_ = this%capacity * 2
            
            ALLOCATE( vals_realloc(this%capacity_), STAT = alloc_stat )
            
            IF( alloc_stat /= 0 ) THEN
                WRITE( *, "(A)" ) "ERROR: in subroutine INSERT: Allocation of vals_realloc &
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
                WRITE( *, "(A)" ) "ERROR: in subroutine INSERT: Reallocation of VECTOR failed!"
                DEALLOCATE( vals_realloc )
                RETURN
            ENDIF
            
            this%vals_ = vals_realloc
            DEALLOCATE( vals_realloc )
        ELSE
            i = this%size_
            DO WHILE( i >= at_ )
                this%vals_(i + 1) = this%vals_(i)
                i = i - 1
            ENDDO
            this%vals_(at_) = val_
            this%size_ = this%size_ + 1
        ENDIF
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE VEC_ERASE( this, at_ )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        INTEGER              , INTENT(IN)    :: at_
        !----- INTERNALS ----------------------------------------------------------------------!
        INTEGER                              :: i
        !----- VARIABLES ----------------------------------------------------------------------!
        
        IF( at_ < 1 .OR. at_ > this%size_ ) RETURN
        
        this%size_ = this%size_ - 1
        
        DO i = at_, this%size_
            this%vals_(i) = this%vals_(i + 1)
        ENDDO
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE VEC_SWAP( this, other )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: other
        !----- INTERNALS ----------------------------------------------------------------------!
        TEMPLATE, DIMENSION(:), ALLOCATABLE  :: copy_vals_this
        TEMPLATE, DIMENSION(:), ALLOCATABLE  :: copy_vals_other
        INTEGER                              :: alloc_stat
        INTEGER                              :: temp
        !----- VARIABLES ----------------------------------------------------------------------!
        
        IF( ALLOCATED( this%vals_ ) ) THEN
            ALLOCATE( copy_vals_this(this%size_), STAT = alloc_stat )
            IF( alloc_stat /= 0 ) THEN
                WRITE( *, "(A)" ) "ERROR: in subroutine VEC_SWAP: Allocation of copy_vals_this &
                &failed!"
                RETURN
            ENDIF
            copy_vals_this = this%vals_
            DEALLOCATE( this%vals_ )
        ENDIF
        
        IF( ALLOCATED( other%vals_ ) ) THEN
            ALLOCATE( copy_vals_other, STAT = alloc_stat )
            IF( alloc_stat /= 0 ) THEN
                WRITE( *, "(A)" ) "ERROR: in subroutine VEC_SWAP: Allocation of copy_vals_other&
                & failed!"
                RETURN
            ENDIF
            copy_vals_other = other%vals_
            DEALLOCATE( other%vals_ )
        ENDIF
        
        ALLOCATE( this%vals_(other%size_), STAT = alloc_stat )
        IF( alloc_stat /= 0 ) THEN
            WRITE( *, "(A)" ) "ERROR: in subroutine VEC_SWAP: Reallocation of this%vals_ &
            &failed!"
            RETURN
        ENDIF
        ALLOCATE( other%vals_(this%size_), STAT = alloc_stat )
        IF( alloc_stat /= 0 ) THEN
            WRITE( *, "(A)" ) "ERROR: in subroutine VEC_SWAP: Realloctaion of other%vals_ &
            &failed!"
            RETURN
        ENDIF
        
        this%vals_      = copy_vals_other
        other%vals_     = copy_vals_this
        temp            = this%size_
        this%size_      = other%size_
        this%capacity_  = this%size_
        other%size_     = temp
        other%capacity_ = other%size_
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE VEC_CLEAR( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        !----- INTERNALS ----------------------------------------------------------------------!
        INTEGER                              :: alloc_stat
        !----- VARIABLES ----------------------------------------------------------------------!
        
        IF( ALLOCATED( this%vals_ ) ) THEN
            DEALLOCATE( this%vals_ )
            this%size_ = 0
            ALLOCATE( this%vals_(this%capacity_), STAT = alloc_stat )
            IF( alloc_stat /= 0 ) THEN
                WRITE( *, "(A)" ) "ERROR: in subroutine VEC_CLEAR: Realloction of this%vals_ &
                &failed!"
                RETURN
            ENDIF
        ELSE
            this%size_ = 0
        ENDIF
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE VEC_RESIZE( this, new_capacity )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        INTEGER              , INTENT(IN)    :: new_capacity
        !----- INTERNALS ----------------------------------------------------------------------!
        TEMPLATE, DIMENSION(:), ALLOCATABLE  :: vals_copy
        INTEGER                              :: alloc_stat
        INTEGER                              :: i
        !----- VARIABLES ----------------------------------------------------------------------!
        
        IF( this%size_ > 0 ) THEN
            ALLOCATE( vals_copy(this%size_), STAT = alloc_stat )
            IF( alloc_stat /= 0 ) THEN
                WRITE( *, "(A)" ) "ERROR: in subroutine VEC_RESIZE: Allocation of vals_copy &
                &failed!"
                RETURN
            ENDIF
            vals_copy = this%vals_
        ENDIF
        
        IF( ALLOCATED( this%vals_ ) ) DEALLOCATE( this%vals_ )
        
        ALLOCATE( this%vals_(new_capacity), STAT = alloc_stat )
        IF( alloc_stat /= 0 ) THEN
            WRITE( *. "(A)" ) "ERROR: in subroutine VEC_RESIZE: Reallocation of this%vals_ &
            &failed!"
            IF( ALLOCATED( vals_copy ) ) DEALLOCATE( vals_copy )
            RETURN
        ENDIF
        
        this%capacity_ = new_capacity
        IF( this%size_ > new_capacity ) this%size_ = new_capacity
        
        DO i = 1, this%size_
            this%vals_(i) = vals_copy(i)
        ENDDO
    END SUBROUTINE
    !..........................................................................................!
    LOGICAL FUNCTION VEC_EMPTY( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(IN) :: this
        !----- VARIABLES ----------------------------------------------------------------------!
        
        IF( this%size_ == 0 ) THEN
            VEC_EMPTY = .TRUE.
            RETURN
        ELSE
            VEC_EMPTY = .FALSE.
            RETURN
        ENDIF
    END FUNCTION
    !..........................................................................................!
    SUBROUTINE VEC_RESERVE( this, new_capacity )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        TYPE(VECTOR_TEMPLATE), INTENT(INOUT) :: this
        INTEGER              , INTENT(IN)    :: new_capacity
        !----- INTERNALS ----------------------------------------------------------------------!
        TEMPLATE, DIMENSION(:), ALLOCATABLE  :: vals_copy
		INTEGER                              :: alloc_stat
        !----- VARIABLES ----------------------------------------------------------------------!
        
		! There is nothing to reserve if the new capacity is equal or lower than the current
		! capacity...
        IF( new_capacity <= this%capacity_ ) RETURN
		
		! Check for reallocation:
		IF( this%size_ /= 0 ) THEN
			ALLOCATE( vals_copy(new_capacity), STAT = alloc_stat )
			IF( alloc_stat /= 0 ) THEN
				WRITE( *, "(A)" ) "ERROR: in subroutine VEC_RESERVE: Allocation of vals_copy &
				&failed!"
				RETURN
			ENDIF
			
		ELSE
			ALLOCATE( this%vals_(new_capacity), STAT = alloc_stat )
			IF( alloc_stat /= 0 ) THEN
				WRITE( *, "(A)" ) "ERROR: in subroutine VEC_RESERVE: Allocation of this%vals_ &
				&failed!"
				RETURN
			ENDIF
			this%capacity_ = new_capacity
		ENDIF
    END SUBROUTINE
    !..........................................................................................!
    SUBROUTINE VEC_SHRINK_TO_FIT( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END SUBROUTINE
    !..........................................................................................!
    TEMPLATE FUNCTION VEC_FRONT( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END FUNCTION
    !..........................................................................................!
    TEMPLATE FUNCTION VEC_BACK( this )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END FUNCTION
    !..........................................................................................!
    TEMPLATE FUNCTION VEC_AT( this, at_ )
        IMPLICIT NONE
        !----- VARIABLES ----------------------------------------------------------------------!
        
        !----- INTERNALS ----------------------------------------------------------------------!
        
        !----- VARIABLES ----------------------------------------------------------------------!
    END FUNCTION
    !----- MODULE SUBROUTINES -----------------------------------------------------------------!