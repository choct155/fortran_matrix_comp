MODULE gauss_elim
	IMPLICIT NONE

CONTAINS

	!***********************************************************************************
	!***Defined a function that returns the simple dot product of a matrix and vector***
	!***Matrix Vector Multiplication Section 1.1 - Row wise*****************************
	!***********************************************************************************

	FUNCTION mvm_row(mat,vec) RESULT (vec_out)
		REAL,DIMENSION(:,:),INTENT(IN)		:: mat 		!Declare input matrix of unknown dimension
		REAL,DIMENSION(:),INTENT(IN)		:: vec 		!Declare input vector of unknown length
		REAL,DIMENSION(SIZE(mat,1))	 		:: vec_out 	!Declare output vector
		INTEGER 							:: i,j 		!Declare iterators

		!Check to see if the dimensions are compatible
		IF(SIZE(mat,2)/=SIZE(vec)) THEN
			STOP 'Sheeeit, these arrays are non-conformable!'
		ENDIF

		!Initialize output vector
		vec_out=0

		!Multiply mat by vec
		DO i=1,SIZE(mat,1) 									!For each row...
			DO j=1,SIZE(mat,2)								!...and each column in mat\position in vec...
				vec_out(i)=vec_out(i)+mat(i,j)*vec(j) 	!...augment the output vector with the product of the indexed positions
			END DO
		END DO

	END FUNCTION mvm_row

	!***********************************************************************************
	!***Defined a function that returns the simple dot product of a matrix and vector***
	!***Matrix Vector Multiplication Section 1.1 - Column wise**************************
	!***********************************************************************************

	FUNCTION mvm_col(mat,vec) RESULT (vec_out)
		REAL,DIMENSION(:,:),INTENT(IN)		:: mat 		!Declare input matrix of unknown dimension
		REAL,DIMENSION(:),INTENT(IN)		:: vec 		!Declare input vector of unknown length
		REAL,DIMENSION(SIZE(mat,1))	 		:: vec_out 	!Declare output vector
		INTEGER 							:: i,j 		!Declare iterators

		!Check to see if the dimensions are compatible
		IF(SIZE(mat,2)/=SIZE(vec)) THEN
			STOP 'Sheeeit, these arrays are non-conformable!'
		ENDIF

		!Initialize output vector
		vec_out=0

		!Multiply mat by vec
		DO j=1,SIZE(mat,2) 									!For each row...
			DO i=1,SIZE(mat,1)								!...and each column in mat\position in vec...
				vec_out(i)=vec_out(i)+mat(i,j)*vec(j) 	!...augment the output vector with the product of the indexed positions
			END DO
		END DO

	END FUNCTION mvm_col

	!***************************************************
	!***Define a function to generate a random vector***
	!***************************************************

	FUNCTION rand_vec(dim) RESULT(vec)
		INTEGER,INTENT(IN) 				:: dim 		!Declare extent of array
		REAL,DIMENSION(:),ALLOCATABLE 	:: vec 		!Declare output vector
		INTEGER 						:: i 		!Declare iterator

		!Allocate space for the array
		ALLOCATE(vec(dim))

		!Populate array
		DO i=1,dim 			!For each vector position...
			vec(i)=rand() 	!...fill it with a random number
		END DO

	END FUNCTION rand_vec

	!***************************************************
	!***Define a function to generate a random matrix***
	!***************************************************

	FUNCTION rand_matrix(dim1,dim2) RESULT(mat)
		INTEGER,INTENT(IN)  				:: dim1,dim2 	!Declare extent of both dimensions
		REAL,DIMENSION(:,:),ALLOCATABLE  	:: mat 			!Declare output matrix
		INTEGER 							:: i,j 			!Declare iterators

		!Allocate space for the array
		ALLOCATE(mat(dim1,dim2))

		!Populate array
		DO i=1,dim1
			DO j=1,dim2
				mat(i,j)=rand()
			END DO
		END DO 

	END FUNCTION rand_matrix

END MODULE gauss_elim