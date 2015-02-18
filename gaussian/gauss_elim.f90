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

	!******************************************************
	!***Define function to multiply to matrices together***
	!******************************************************

	FUNCTION mat_mult(mat1,mat2) RESULT(mat_out)
		REAL,DIMENSION(:,:),INTENT(IN) 		:: mat1 		!Declare first input matrix
		REAL,DIMENSION(:,:),INTENT(IN) 		:: mat2 		!Declare second input matrix
		REAL,DIMENSION(:,:),ALLOCATABLE		:: mat_out 		!Declare output matrix
		INTEGER 							:: dim1_row 	!Declare extent of first dim in first matrix
		INTEGER 							:: dim1_col 	!Declare extent of second dim in first matrix
		INTEGER 							:: dim2_row 	!Declare extent of first dim in second matrix
		INTEGER 							:: dim2_col 	!Declare extent of second dim in second matrix
		INTEGER 							:: i,j,k		!Declare iterators

		!Capture dimensions of input matrices
		dim1_row=SIZE(mat1,1)
		dim1_col=SIZE(mat1,2)
		dim2_row=SIZE(mat2,1)
		dim2_col=SIZE(mat2,2)

		!Check to see if the dimensions are compatible
		IF(dim1_col/=dim2_row) THEN
			STOP 'Sheeeit, these arrays are non-conformable!'
		ENDIF

		!Allocate space for output matrix
		ALLOCATE(mat_out(dim1_row,dim2_col))
		mat_out=0

		!Multiply matrices
		DO i=1,dim1_row 	!For each row in matrix 1...
			DO j=1,dim2_col 	!...and each column in matrix 2...
				DO k=1,dim2_row 	!...and each element in the jth column of matrix 2 and the ith column of matrix 1...
					mat_out(i,j)=mat_out(i,j)+mat1(i,k)*mat2(k,j) !...add the product of the elements to position i,j in the output matrix
				END DO
			END DO
		END DO 

	END FUNCTION mat_mult





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

	!****************************************************
	!***Define a function to add two matrices together***
	!****************************************************

	FUNCTION mat_add(mat1,mat2) RESULT(mat_out)
		REAL,DIMENSION(:,:),INTENT(IN) 		:: mat1 		!Declare first input matrix
		REAL,DIMENSION(:,:),INTENT(IN) 		:: mat2 		!Declare second input matrix
		REAL,DIMENSION(:,:),ALLOCATABLE		:: mat_out 		!Declare output matrix
		INTEGER 							:: dim1_row 	!Declare extent of first dim in first matrix
		INTEGER 							:: dim1_col 	!Declare extent of second dim in first matrix
		INTEGER 							:: dim2_row 	!Declare extent of first dim in second matrix
		INTEGER 							:: dim2_col 	!Declare extent of second dim in second matrix
		INTEGER 							:: i,j 			!Declare iterators

		!Capture dimensions of input matrices
		dim1_row=SIZE(mat1,1)
		dim1_col=SIZE(mat1,2)
		dim2_row=SIZE(mat2,1)
		dim2_col=SIZE(mat2,2)

		!Allocate space for the output matrix
		ALLOCATE(mat_out(dim1_row,dim1_col))
		mat_out=0

		!Check to see if the dimensions are compatible
		IF((dim1_row/=dim2_row).AND.(dim1_col/=dim2_col)) THEN
			STOP 'Sheeeit, these arrays are non-conformable!'
		ENDIF		

		DO i=1,dim1_row								!For each row...
			DO j=1,dim1_col							!...and each column in the input matrices...
				mat_out(i,j)=mat1(i,j)+mat2(i,j) 	!...add the corresponding positions together
			END DO
		END DO

	END FUNCTION mat_add

	!******************************************
	!***Define a function to negate a matrix***
	!******************************************

	FUNCTION mat_neg(mat_in) RESULT(mat_out)
		REAL,DIMENSION(:,:),INTENT(IN) 		:: mat_in 		!Declare input matrix
		REAL,DIMENSION(:,:),ALLOCATABLE 	:: mat_out 		!Declare output matrix
		INTEGER 							:: i,j 			!Declare iterators

		!Allocate output matrix
		ALLOCATE(mat_out(SIZE(mat_in,1),SIZE(mat_in,2)))

		!Negate matrix
		DO i=1,SIZE(mat_in,1)
			DO j=1,SIZE(mat_in,2)
				mat_out(i,j)=mat_in(i,j)*(-1)
			END DO
		END DO

	END FUNCTION mat_neg



	!************************************************
	!***Define a function to subtract two matrices***
	!************************************************

	FUNCTION mat_subtract(mat1,mat2) RESULT(mat_out)
		REAL,DIMENSION(:,:),INTENT(IN) 		:: mat1 		!Declare first input matrix
		REAL,DIMENSION(:,:),INTENT(IN) 		:: mat2 		!Declare second input matrix
		REAL,DIMENSION(:,:),ALLOCATABLE		:: mat2_neg		!Declare output matrix
		REAL,DIMENSION(:,:),ALLOCATABLE		:: mat_out 		!Declare output matrix
		INTEGER 							:: dim1_row 	!Declare extent of first dim in first matrix
		INTEGER 							:: dim1_col 	!Declare extent of second dim in first matrix
		INTEGER 							:: dim2_row 	!Declare extent of first dim in second matrix
		INTEGER 							:: dim2_col 	!Declare extent of second dim in second matrix
		INTEGER 							:: i,j 			!Declare iterators

		!Capture dimensions of input matrices
		dim1_row=SIZE(mat1,1)
		dim1_col=SIZE(mat1,2)
		dim2_row=SIZE(mat2,1)
		dim2_col=SIZE(mat2,2)

		!Allocate space for the output matrix
		ALLOCATE(mat2_neg(dim2_row,dim2_col))
		ALLOCATE(mat_out(dim1_row,dim1_col))
		mat2_neg=0
		mat_out=0

		!Check to see if the dimensions are compatible
		IF((dim1_row/=dim2_row).AND.(dim1_col/=dim2_col)) THEN
			STOP 'Sheeeit, these arrays are non-conformable!'
		ENDIF		

		!Negate the second matrix
		mat2_neg=mat_neg(mat2)

		!Add the first matrix to the negated second matrix
		mat_out=mat_add(mat1,mat2_neg)

	END FUNCTION mat_subtract

END MODULE gauss_elim