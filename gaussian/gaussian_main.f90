PROGRAM gaussian
    USE gauss_elim
    USE array_inspect
    
    IMPLICIT NONE
    
    !Declare utility variables
    REAL,DIMENSION(:,:),ALLOCATABLE      	:: arr2d                  !Declare allocatable matrix
    REAL,DIMENSION(:),ALLOCATABLE        	:: arr1d                  !Declare allocatable vector
    REAL,DIMENSION(:,:),ALLOCATABLE         :: rmat1,rmat2,rmat3      !Declare three random matrices
    REAL,DIMENSION(:,:),ALLOCATABLE         :: arr_ones               !Declare allocatable matrix for ones
    INTEGER                                 :: i,j                    !Declare iterators
    CHARACTER*1 colnames(5) / 'A','B','C','D','E' /                   !Declare matrix col names
    
    !Declare outputs
    REAL,DIMENSION(:),ALLOCATABLE        	:: rprod    !Declare vector output for row multiplication
    
    !Allocate space for arrays
    ALLOCATE(arr2d(4,5),arr1d(5))
    ALLOCATE(rprod(SIZE(arr2d,1)))
    ALLOCATE(arr_ones(10,4))
   

    !Initialize arrays
    arr2d=0
    arr1d=0
    arr_ones=1
    
    !Populate arrays
    DO i=1,SIZE(arr2d,1)            !For each row...
        DO j=1,SIZE(arr2d,2)        !...and each column...
            arr2d(i,j)=i*j          !...fill the intersection with the product of indices
        END DO
    END DO
    
    DO i=1,SIZE(arr1d)              !For each vector position...
        arr1d(i)=i                  !...fill with the index value
    END DO
    
    !Print arr1d
    PRINT *, "Behold!  It's the 1D vector!"
    CALL print_arr(arr1d)
    
    !Print arr2d
    PRINT *, "We have 2D in the repertoire as well!"
    CALL print_arr(arr2d,colnames)
 
    !Capture the dot product of the arrays (arr2d*arr1d)
    PRINT *, "How does the row-wise inner product function perform?"
    rprod=mvm_row(arr2d,arr1d)
    CALL print_vec(rprod)
    
    !Test the random vector function
    PRINT *, 'Here are two random 2D arrays...'
    rmat1=rand_matrix(10,4)
    rmat2=rand_matrix(4,5)
    CALL print_arr(rmat1)
    CALL print_arr(rmat2)

    !Calculate their product
    PRINT *, 'Observe the product of these random arrays:'
    CALL print_arr(mat_mult(rmat1,rmat2))
    
    !Add two random matrices together by repurposing rmat1/2
    PRINT *, 'Observe summation of two instances of the first matrix:'
    DEALLOCATE(rmat2)           !Reallocate second random matrix
    ALLOCATE(rmat2(10,4))
    rmat2=rmat1                 !Make second matrix a copy of the first
    rmat3=mat_add(rmat1,rmat2)  !Add the identical matrices together
    CALL print_arr(rmat3)       

    !Subtract a matrix of ones from the result
    PRINT *, 'Here is the summary matrix less one:'
    CALL print_arr(mat_subtract(rmat3,arr_ones))

    !Determinant of a 2x2 matrix
    PRINT *, 'Observe the random 2x2 matrix...'
    rmat1=rand_matrix(2,2)
    CALL print_arr(rmat1)
    
    PRINT *, 'This is the determinant of the aformentioned random 2x2 matrix'
    PRINT *, mat_det(rmat1)


    rmat2=rand_matrix(5,5)
    CALL print_arr(rmat2)
    CALL print_arr(rmat2(1:2,3:))
    CALL print_arr(rmat2((/ 1,3,5 /),(/2,4/)))


END PROGRAM gaussian