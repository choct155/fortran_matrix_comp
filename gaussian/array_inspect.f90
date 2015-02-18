!This module captures print and diagnostic functions
MODULE array_inspect
    IMPLICIT NONE
    
    !!!THE PRINTING APPROACH WILL COLLAPSE PRINTING FOR DIFFERENT OBJECTS
    !!!INTO A SINGLE FUNCTION: print_arr
    !Define wrapper for print functions (multiple dispatch by array rank)
    INTERFACE print_arr
        MODULE PROCEDURE print_vec      !For RANK=1
        MODULE PROCEDURE print_mat      !For RANK=2
    END INTERFACE print_arr
    
    CONTAINS
    
    !**************************************
    !***Define routine to print a vector***
    !**************************************
    
    SUBROUTINE print_vec(vec)
        REAL,DIMENSION(:),INTENT(IN)     :: vec  !Declare input vector
        INTEGER                          :: i    !Declare iterator
        
        !Print header
        PRINT *,''
        PRINT '(a10,a,a10)', 'INDEX',',','VALUE'
        PRINT '(2a11)','---------','---------'
        
        !Print vector
        DO i=1,SIZE(vec)        
            PRINT '(i10,a,f10.3)',i,',',vec(i)
        END DO
        PRINT *,''
        
    END SUBROUTINE print_vec
    
    !*****************************************
    !***Define routine to print a 2d matrix***
    !*****************************************
    
    SUBROUTINE print_mat(mat,names)
        REAL,DIMENSION(:,:),INTENT(IN)      :: mat      !Declare input matrix
        CHARACTER,DIMENSION(:),OPTIONAL     :: names    !Declare names
        INTEGER                             :: i,j      !Declare iterators
        
        IF(PRESENT(names)) THEN     !If column headers were supplied...
            PRINT *,''
            PRINT '(200(a10,a))','INDEX',',',(names(i),',',i=1,SIZE(names)) !...use them in the matrix header...
            PRINT '(200a11)',('---------',i=1,SIZE(names)+1) !...delineate header from data...
            DO i=1,SIZE(mat,1)   !...and for each row...
                PRINT '(i10,a,200(f10.3,a))',i,',',(mat(i,j),',',j=1,SIZE(mat,2))  !...print the data valyes
            END DO
            PRINT *,''
        ELSE 
            PRINT *,''
            PRINT '(a10,a,200(i10,a))','INDEX',',',(i,',',i=1,SIZE(mat,2))  !...otherwise use integers for the column header...
            PRINT '(200a11)',('---------',i=1,SIZE(mat,2)+1)    !...and proceed similarly
            DO i=1,SIZE(mat,1)
                PRINT '(i10,a,200(f10.3,a))',i,',',(mat(i,j),',',j=1,SIZE(mat,2))
            END DO
            PRINT *,''
        ENDIF
    END SUBROUTINE print_mat
    
    !***************************************************************
    !***Define function to return the determinant of a 2x2 matrix***
    !***************************************************************

    !An excellent discussion of the significance of determinants can be found here:  http://math.stackexchange.com/questions/668/whats-an-intuitive-way-to-think-about-the-determinant?newreg=8998d19e356e4ef4a62dff7ad37f33e4

    FUNCTION mat_det(mat_in) RESULT(dtr)
        REAL,DIMENSION(:,:),INTENT(IN)      :: mat_in   !Declare input matrix
        REAL                                :: dtr      !Declare determinant
        REAL                                :: a,b,c,d  !Declare matrix positions

        !Check if the input matrix is 2x2
        IF ((SIZE(mat_in,1)/=2).OR.(SIZE(mat_in,2)/=2)) THEN
            STOP 'The input matrix is not 2x2.'
        END IF

        !Check if the input matrix is square
        IF (SIZE(mat_in,1)/=SIZE(mat_in,2)) THEN
            STOP "This matrix ain't even square yo.  Get this JV shit outta here..."
        END IF

        !Assign values to each matrix position
        a=mat_in(1,1)
        b=mat_in(1,2)
        c=mat_in(2,1)
        d=mat_in(2,2)

        !Calculate determinant
        dtr=(a*d)-(b*c)

    END FUNCTION mat_det

    !***********************************************************************
    !***Define function to calculate determinants of any 2D square arrays***
    !***********************************************************************

    !We want to be able to handle an array of any size.  Since we don't know
    !the size of the array at the outset, we will use recursion to wittle our 
    !way down to a 2x2 matrix. Then we can use `mat_det()` to return a result.

    !There are a few things with which we must contend. Since we don't know 
    !the size of the array, we do not know the number sub-operations are 
    !generated at each loop.  For example, in a 3x3 matrix situation, it 
    !decomposes into three sub-operations.  This creates a few issues:
    !   1.  We don't know a priori how many sub-ops are required, but this is 
    !       easily identified as the number of columns.
    !   2.  We don't know how many containers to declare.
    !   3.  We need to operate on each of an unknown number of these subops.
    !   4a. Values will need to be preserved across recursion loops for 
    !       subsequent calculation.  The basic decomposition is a scalar (from)
    !       the first row times the determinant of a smaller matrix (including
    !       the columns that are not shared with the scalar).  That smaller 
    !       matrix may not be 2x2 yet, so the scalar must be preserved for the 
    !       purpose of being multiplied by the evaluated determinant of its
    !       associated, smaller matrix.
    !   4b. Alternatively, we could preprocess the smaller matrix to yield a
    !       multiple of the determinant that is consistent with the scalar. For 
    !       example, suppose we have a matrix A:
    !
    !           2   0
    !           0   2
    !
    !       We want to multiply the determinant (4) by a scalar x=4. Since A has
    !       not yet been processed, we can modify the underlying matrix to 
    !       achieve the same result.  A2=A*2:
    !
    !           4   0
    !           0   4
    !
    !       Note that this square root relationship does not hold.  Observe B:
    !
    !           3   0   0
    !           0   3   0
    !           0   0   3
    !
    !       The determinant here is 27.  (Tip: Diagonal matrices have determinants
    !       equal to the product of the values on the diagonal.  This is true for
    !       lower and upper matrices because the determinant is robust to row
    !       permutations.  This intuitively makes sense because the scalar/small
    !       determinant decomposition would yield only one pair with a non-zero
    !       scalar.)  Suppose we want to multiply the result by 8, giving us a 
    !       product of 216.  The same value could be achieved with B2=B*2:
    !
    !           6   0   0
    !           0   6   0
    !           0   0   6
    !
    !       Thus, the determinant impact of multiplying a matrix by a constant is
    !       c^n, where c=constant & n=number of rows in the matrix.
    !

    FUNCTION mat_det3(mat_in) RESULT(dtr)
        REAL,DIMENSION(:,:),INTENT(IN)      :: mat_in   !Declare input matrix
        REAL                                :: dtr      !Declare determinant
        INTEGER                             :: i        !Declare iterator

        !Check if the input matrix is square
        IF (SIZE(mat_in,1)/=SIZE(mat_in,2)) THEN
            STOP "This matrix ain't even square yo.  Get this JV shit outta here..."
        END IF

        !Check if the input matrix is 2x2
        IF ((SIZE(mat_in,1)/=2).OR.(SIZE(mat_in,2)/=2)) THEN
            PRINT *, mat_det(mat_in)
        ENDIF
        !ELSE
            !DO i=1,SIZE(mat_in,2)

    END FUNCTION mat_det3



END MODULE array_inspect
    
    