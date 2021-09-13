PROGRAM main
	USE VECTOR_INTEGER
	
	IMPLICIT NONE
	TYPE(VECTOR) :: int_vector
	INTEGER      :: i
	
	WRITE(*, *) "VECTOR_CAPACITY : ", int_vector%capacity_
	WRITE(*, *) "VECTOR_SIZE     : ", int_vector%size_
	
	DO i = 1, 10
		CALL PUSH_BACK(int_vector, i)
	ENDDO
	
	WRITE(*, *) "VECTOR_CAPACITY : ", int_vector%capacity_
	WRITE(*, *) "VECTOR_SIZE     : ", int_vector%size_
	WRITE(*, *) int_vector%vals_(:)
	
END PROGRAM main