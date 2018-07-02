# This file contains a collection of functions that can cache the inverse of a
# matrix in a separate environment

## this function sets up a caching environment that stores a matrix and its 
## inverse, and functions to read and write to the caching environment

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## this function either retrieves the cached inverse, or when it does not exist
## calculates and caches the inverse, then returns it

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
