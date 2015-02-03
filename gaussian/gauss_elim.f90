MODULE gauss_elim
	IMPLICIT NONE

CONTAINS

	!Defined a function that returns the simple dot product of a matrix and vector
	!Matrix Vector Multiplication Section 1.1

	FUNCTION mvm_row(mat,vec) RESULT (vec_out)
		INTEGER,DIMENSION(:,:),INTENT(IN)	:: mat 		!Declare input matrix of unknown dimension
		INTEGER,DIMENSION(:),INTENT(IN)		:: vec 		!Declare input vector of unknown length
		INTEGER,DIMENSION(SIZE(mat,1))	 	:: vec_out 	!Declare output vector
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


END MODULE gauss_elim