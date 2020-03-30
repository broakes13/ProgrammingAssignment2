## Set of functions that computes the inverse of a matrix if it
## has not already been computed.  If previously computed, the
## functions retrieve result from the cache.


## This function creates a special "matrix" object
##    that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmat <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmat <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(setmat = setmat, getmat = getmat,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getmat()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
