## Programming Assignment 2: Lexical Scoping
## William

## This function takes a matrix as input, and gives an inverse of this matrix as 
## a result and stores it. If this inverse of this matrix has been calculated,
## then get it from the storage and print "getting cached data".

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to do the follow things


makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        ## set the value of the matrix
        set <- function(y) {  
                x <<- y
                I <<- NULL
        }
        ## get the value of the matrix
        get <- function() {
                x
        }
        ## set the value of the inverse
        setinverse <- function(inverse){
                I <<- inverse
        }
        ## get the value of the inverse
        getinverse <- function() {
                I
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse of 
## the matrix has already been calculated. If so, it gets the inverse of the 
## matrix from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse of the matrix in 
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        ## if the inverse of matrix hasn't been calculated calculate it by 
        ## solve()
        I <- solve(data, ...)
        x$setinverse(I)
        I
        ## Return a matrix that is the inverse of 'x'
}

## PS: Some comments refer from the Introduction of ProgrammingAssignment2
