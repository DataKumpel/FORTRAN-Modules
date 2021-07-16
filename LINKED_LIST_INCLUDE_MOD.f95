!//////////////////////////////////////////////////////////////////////////////////////////////////!
!/                                                                                                /!
!/ LINKED LIST INCLUDE MODs                                                                       /!
!/                                                                                                /!
!/ To define the linked list for a certain type, it must be defined into the TEMPLATE define.     /!
!/ See below examples on how to do this.                                                          /!
!/                                                                                                /!
!/ Compile with preprocessing enabled to create the type specific modules.                        /!
!/ Do so like this:                                                                               /!
!/                                                                                                /!
!/     gfortran LINKED_LIST_INCLUDE_MOD.f95 -o ll_include.o -cpp -ffree-line-length-none          /!
!/                                                                                                /!
!//////////////////////////////////////////////////////////////////////////////////////////////////!

MODULE LINKED_LIST_INTEGER_MOD
    IMPLICIT NONE
    SAVE
    
#define TEMPLATE             INTEGER
#define NODE_TEMPLATE        NODE_INTEGER
#define LINKED_LIST_TEMPLATE LINKED_LIST_INTEGER

#include "LINKED_LIST_TEMPLATE_MOD.f95"

#undef TEMPLATE
#undef NODE_TEMPLATE
#undef LINKED_LIST_TEMPLATE
END MODULE

MODULE LINKED_LIST_REAL_MOD
    IMPLICIT NONE
    SAVE
    
#define TEMPLATE             REAL
#define NODE_TEMPLATE        NODE_REAL
#define LINKED_LIST_TEMPLATE LINKED_LIST_REAL

#include "LINKED_LIST_TEMPLATE_MOD.f95"

#undef TEMPLATE
#undef NODE_TEMPLATE
#undef LINKED_LIST_TEMPLATE
END MODULE

!##################################################################################################!

PROGRAM TEST_UNIT
    USE LINKED_LIST_INTEGER_MOD
    USE LINKED_LIST_REAL_MOD
    IMPLICIT NONE
    
END PROGRAM