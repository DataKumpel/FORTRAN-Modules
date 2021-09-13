

TYPE :: VECTOR
	TEMPLATE, DIMENSION(:), ALLOCATABLE :: vals_
	INTEGER                             :: size_
	INTEGER                             :: capacity_
END TYPE VECTOR

CONTAINS
	SUBROUTINE PUSH_BACK(this, val_)
		IMPLICIT NONE
		!----- INPUT / OUTPUT VARIABLES -----------------------------------------------------------!
		TYPE(VECTOR), INTENT(INOUT) :: this
		TEMPLATE    , INTENT(IN)    :: val_
		!----- INPUT / OUTPUT VARIABLES -----------------------------------------------------------!
		
		!----- LOCAL VARIABLES --------------------------------------------------------------------!
		TEMPLATE, DIMENSION(:), ALLOCATABLE :: vals_realloc
		INTEGER                             :: alloc_stat
		INTEGER                             :: i
		!----- LOCAL VARIABLES --------------------------------------------------------------------!
		
		IF(this%size_ >= this%capacity_) THEN
			this%capacity_ = this%capacity_ * 2
			
			ALLOCATE(vals_realloc(this%capacity_), STAT = alloc_stat)
			
			IF(alloc_stat /= 0) THEN
				WRITE(*, "(A)") "ERROR: in subroutine PUSH_BACK: Allocation of vals_realloc failed!"
				RETURN
			ENDIF
			
			! copy all values from the vector:
			DO i = 1, this%size_
				vals_realloc(i) = this%vals_(i)
			ENDDO
			
			this%size_ = this%size_ + 1
			vals_realloc(this%size_) = val_
			
			DEALLOCATE(this%vals_)
			ALLOCATE  (this%vals_(this%capacity_), STAT = alloc_stat)
			
			IF(alloc_stat /= 0) THEN
				WRITE(*, "(A)") "ERROR: in subroutine PUSH_BACK: Reallocation of VECTOR failed!"
				RETURN
			ENDIF
			
			this%vals_ = vals_realloc
		ELSE
			this%size_ = this%size_ + 1
			this%vals_(this%size_) = val_
		ENDIF
	END SUBROUTINE PUSH_BACK