## Functions related to a special matrix which caches its inverse

## This creates a special matrix which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Get the inverse of the matrix, return cached value if possible

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    i <- solve(x$get(), ...)
    x$setinv(i)
    i
}
