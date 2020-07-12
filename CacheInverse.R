#########################################################################
#Programming Assignment 2
#Purpose: Caching the Inverse of a Matrix
#Functions defined in this code assumes that the matrix supplied 
#is always invertible.
#########################################################################

#makeCacheMatrix: This function creates a special "matrix"
#object that can cache its inverse.

makeCacheMatrix <- function(MAT = matrix()) {
    MATINV <- NULL
    setMAT <- function(y) {
        MAT <<- y
        MATINV <<- NULL
    }
    getMAT <- function() MAT
    setINV <- function(solve) MATINV <<- solve
    getINV <- function() MATINV
    list(setMAT = setMAT, getMAT = getMAT,
         setINV = setINV,
         getINV = getINV)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then the cachesolve should retrieve the inverse from
#the cache.

cacheSolve <- function(x, ...) {
    MATINV <- x$getINV()
    if(!is.null(MATINV)) {
        message("getting cached data")
        return(MATINV)
    }
    data <- x$getMAT()
    MATINV <- solve(data, ...)
    x$setINV(MATINV)
     MATINV
}

#Solved Example
M=matrix(c(1,0.3,0.3,0.3,1,0.3,0.3,0.3,1),ncol=3)
D=makeCacheMatrix(M)
cacheSolve(D)
