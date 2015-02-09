PROGRAM gaussian
    USE gauss_elim
    USE array_inspect
    
    IMPLICIT NONE
    
    !Declare utility variables
    REAL,DIMENSION(:,:),ALLOCATABLE      	:: arr2d    !Declare allocatable matrix
    REAL,DIMENSION(:),ALLOCATABLE        	:: arr1d    !Declare allocatable vector
    INTEGER                                 :: i,j      !Declare iterators
    CHARACTER*1 colnames(5) / 'A','B','C','D','E' /     !Declare matrix col names
    
    !Declare outputs
    REAL,DIMENSION(:),ALLOCATABLE        	:: rprod    !Declare vector output for row multiplication
    
    !Allocate space for arrays
    ALLOCATE(arr2d(4,5),arr1d(5))
    ALLOCATE(rprod(SIZE(arr2d,1)))
    
    !Initialize arrays
    arr2d=0
    arr1d=0
    
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
    
    CALL print_arr(rand_matrix(10,7))
END PROGRAM gaussian