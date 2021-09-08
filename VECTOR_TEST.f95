PROGRAM main
	USE VECTOR_INTEGER_MOD
    USE VECTOR_REAL_MOD
	
	IMPLICIT NONE
	TYPE( VECTOR_INTEGER ) :: int_vector
    TYPE( VECTOR_REAL )    :: real_vector
	INTEGER                :: i
	
	WRITE(*, *) "VECTOR_CAPACITY : ", int_vector%capacity_
	WRITE(*, *) "VECTOR_SIZE     : ", int_vector%size_
	
	DO i = 1, 10
		CALL PUSH_BACK( int_vector, i )
        CALL PUSH_BACK( real_vector, i * 0.1 )
	ENDDO
	
	WRITE(*, *) "INT_VECTOR_CAPACITY : ", int_vector%capacity_
	WRITE(*, *) "INT_VECTOR_SIZE     : ", int_vector%size_
	WRITE(*, *) int_vector%vals_(:)
    
    WRITE(*, *) "REAL_VECTOR_CAPACITY : ", real_vector%capacity_
	WRITE(*, *) "REAL_VECTOR_SIZE     : ", real_vector%size_
	WRITE(*, *) real_vector%vals_(:)
	
END PROGRAM main