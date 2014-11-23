## These functions calculate the inverse of a matrix 
## and store it in cache

## makeCacheMatrix uses 4 functions that can cache
## a given matrix 'x' and its inverse 'inv'

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #function 'set' stores a new value for 'x' 
    set <- function(y) {
        x <<- y
        inv <<- NULL #and resets 'inv'
    }
    # function 'get' returns 'x'
    get <- function() x
    # function 'setinv' stores 'inv'
    setinv <- function(inverse) inv <<- inverse
    # function 'getinv' returns 'inv'
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calculates the inverse of matrix 'x',
## if it does not exist already, and stores it to cache

cacheSolve <- function(x, ...) {
    ## get 'inv' value from cache
    inv <- x$getinv()
    ## check if 'inv' is not NULL and return its value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if 'inv' is NULL, calculate it from 'x'
    data <- x$get()
    inv <- solve(data, ...)
    ## cache 'inv'
    x$setinv(inv)
    ## print 'inv'
    inv
}
