PROGRAM MAIN
	IMPLICIT NONE
	INTEGER                            :: num_vals
	INTEGER                            :: gcd_result
	INTEGER                            :: alloc_stat
	INTEGER                            :: i
	INTEGER, DIMENSION(:), ALLOCATABLE :: vals
	CHARACTER(LEN = 32)                :: val_in
	
	INTERFACE
		FUNCTION FIND_GCD( nums, numsSize ) RESULT( res )
			INTEGER                      , INTENT(IN) :: numsSize
			INTEGER, DIMENSION(numsSize) , INTENT(IN) :: nums
			INTEGER                                   :: res
		END FUNCTION
	END INTERFACE
	
	num_vals = IARGC()
	
	IF( num_vals == 0 ) STOP "No arguments were given..."
	
	ALLOCATE( vals(num_vals), STAT = alloc_stat )
	IF( alloc_stat /= 0 ) STOP "ERROR: Allocation of value array failed..."
	
	DO i = 1, num_vals
		CALL GETARG( i, val_in )
		READ( val_in, "(I32)" ) vals(i)
	ENDDO
	
	WRITE( *, * ) vals(:)
	WRITE( *, * ) "Find gcd: ", FIND_GCD( vals, num_vals )
END PROGRAM

FUNCTION FIND_GCD( nums, numsSize ) RESULT( res )
	IMPLICIT NONE
	INTEGER                      , INTENT(IN) :: numsSize
	INTEGER, DIMENSION(numsSize) , INTENT(IN) :: nums
	INTEGER                                   :: res
	
	INTERFACE
		FUNCTION GCD( m, n ) RESULT( answer )
			INTEGER, INTENT(IN) :: m, n
			INTEGER             :: answer
		END FUNCTION
	END INTERFACE
	
	res = GCD( MINVAL( nums ), MAXVAL( nums ) )
	RETURN
END FUNCTION

FUNCTION GCD( m, n ) RESULT( answer )
	IMPLICIT NONE
	INTEGER, INTENT(IN) :: m, n
	INTEGER             :: answer, irest, ifirst
	
	ifirst = IABS( m )
	answer = IABS( n )
	
	IF( answer == 0 ) THEN
		answer = ifirst
	ELSE
		DO
			irest = MOD( ifirst, answer )
			IF( irest == 0 ) EXIT
			ifirst = answer
			answer = irest
		ENDDO
		answer = IABS( answer )
	ENDIF
	RETURN
END FUNCTION