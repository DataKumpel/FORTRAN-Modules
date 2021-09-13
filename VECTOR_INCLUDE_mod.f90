
MODULE VECTOR_INTEGER
	IMPLICIT NONE
	SAVE

#define TEMPLATE INTEGER
#include "VECTOR_mod.f95"
#undef TEMPLATE
END MODULE VECTOR_INTEGER

!--------------------------------------------------------------------------------------------------!

MODULE VECTOR_REAL
	IMPLICIT NONE
	SAVE

#define TEMPLATE REAL
#include "VECTOR_mod.f95"
#undef TEMPLATE
END MODULE VECTOR_REAL