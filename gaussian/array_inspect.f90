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
    
END MODULE array_inspect
    
    