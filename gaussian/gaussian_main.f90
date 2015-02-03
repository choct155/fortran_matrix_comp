PROGRAM gaussian
	USE gauss_elim

	IMPLICIT NONE

	!Declare utilities
	INTEGER,DIMENSION(:,:),ALLOCATABLE 		:: arr2d 	!Declare allocatable matrix
	INTEGER,DIMENSION(:),ALLOCATABLE 		:: arr1d 	!Declare allocatable vector
	INTEGER 								:: i,j 		!Declare iterators

	!Declare outputs
	INTEGER,DIMENSION(:),ALLOCATABLE 		:: rprod 	!Declare vector output for row multiplication

	!Allocate space for arrays
	ALLOCATE(arr2d(4,5),arr1d(5))
	ALLOCATE(rprod(SIZE(arr2d,1)))

	!Initialize arrays
	arr2d=0
	arr1d=0

	!Populate arrays
	DO i=1,SIZE(arr2d,1)		!For each row...
		DO j=1,SIZE(arr2d,2)	!...and each column...
			arr2d(i,j)=i*j 		!...fill the intersection with the index product
		END DO
	END DO

	DO i=1,SIZE(arr1d) 			!For each vector position...
		arr1d(i)=i  			!...fill with the index value
	END DO
	
	!Capture the dot product of the arrays (arr2d*arr1d)
	rprod=mvm_row(arr2d,arr1d)

	!Print arr2d
	PRINT *, SIZE(arr2d)

END PROGRAM