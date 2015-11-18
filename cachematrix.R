## Inverting a matrix tends to use a lot of computing power and can thus slow
## down processes. By saving the cache of a inverse matrix, we can manipulate
## both the matrix and its inverse as an object instead of recomputing their
## parameter.

## This function creates an object (matrix) from which we can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(Tinverse) inverse <<- Tinverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function creates the inverse of the matrix from the function above and then
## caches it for later retrieval.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrixdata <- x$get()
    inverse <- solve(matrixdata, ...)
    x$setinverse(inverse)
    inverse
}
