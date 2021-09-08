
MODULE VECTOR_INTEGER_MOD
	IMPLICIT NONE
	SAVE

#define TEMPLATE        INTEGER
#define VECTOR_TEMPLATE VECTOR_INTEGER

#include "VECTOR_mod.f95"

#undef TEMPLATE
#undef VECTOR_TEMPLATE
END MODULE

!--------------------------------------------------------------------------------------------------!

MODULE VECTOR_REAL_MOD
	IMPLICIT NONE
	SAVE

#define TEMPLATE REAL
#define VECTOR_TEMPLATE VECTOR_REAL

#include "VECTOR_mod.f95"

#undef TEMPLATE
#undef VECTOR_TEMPLATE
END MODULE