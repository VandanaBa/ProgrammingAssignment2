## makeCacheMatrix function creates a matrix that can cache its inverse.
## it does the following functions- 

## makeCacheMatrix set the value of matrix, get the value of matrix, set the value of inverse of matrix, and get the value of inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}



## The cacheSolve function calculates the inverse of the  "matrix".
## cacheSolve Computes, caches, and returns   matrix inverse
## It first checks  whether the inverse of matrix is already calculated.  If it is available, it returns cached matrix inverse using ##previously computed matrix inverse getting cached data
## Using x$setinverse(i), you can modify existing matrix and then using cacheSolve(), you can compute, cache, and return new matrix inverse.
## x$get()	returns matrix and x$getinverse() returns matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 i <- x$getinverse() 
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()		
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## how it operates
## mymatrix = makeCacheMatrix(matrix(c(4,5,6,7), nrow=2, ncol=2)) # to create original matrix

##> mymatrix$get()    # gives matrix created
##     [,1] [,2]
##[1,]    4    6
##[2,]    5    7

## cacheSolve(mymatrix)   # Computes, caches, and returns    matrix inverse
##    [,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2

##> mymatrix$getinverse()  # gives matrix inverse
##     [,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2

## cacheSolve(mymatrix)   # Returns cached matrix inverse using previously computed matrix inverse getting cached data
##     [,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2

##mymatrix$set(matrix(c(12,15,77,74), nrow=2, ncol=2)) # to modify existing matrix

## cacheSolve(mymatrix)   # Computes, caches, and returns new matrix inverse
##           [,1]        [,2]
##[1,] -0.27715356  0.28838951
##[2,]  0.05617978 -0.04494382

##> mymatrix$get()         # Returns matrix
 ##    [,1] [,2]
##[1,]   12   77
##[2,]   15   74

##> mymatrix$getinverse()  # Returns matrix inverse
##            [,1]        [,2]
##[1,] -0.27715356  0.28838951
##[2,]  0.05617978 -0.04494382
