## Citing from assignment 3 instructions, "Matrix inversion is usually a costly
## computation and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly."


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    xInverse <- NULL     # Initialize the inverse matrix to NULL
    set <- function(y) {
        x <<- y # set new value of matrix
        xInverse <- NULL # set inverse to NULL 
    }
    get <- function(){ x } # return the matrix
    getInverse<- function() xInverse # return the inverse of matrix
    setInverse<- function(inverted) xInverse <<- inverted # new value of inverse is cached
    list(set = set, get = get, getInverse = getInverse, setInverse=setInverse)    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the inverse 
## from the cache
cacheSolve <- function(x, ...) {
    inv <- x$getInverse() # get the current value of xInverse
    if(!is.null(inv)) { # if the value is available (already cached) ...
        message("getting cached data")
        return(inv) # ...then return it
    }
    # else (inverse not available)
    matrix <- x$get() # get the matrix
    # assuming that the matrix supplied is always invertible ...
    inv <- solve(matrix, ...) # ... invert it
    x$setInverse(inv) # cache it anew
    inv # and return it
}
