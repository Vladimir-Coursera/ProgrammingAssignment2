## This functions allows to calculate inverse of matrix and cache the results.

## This function creates a list of functions that are allows caching
## inverse of matrix.
## Use:
##  'set' to set the value of the matrix;
##  'get' to get the value of the matrix;
##  'setinverse' to set the inverse of matrix;
##  'getinverse' to get the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the matrix created with the
## 'makeCacheMatrix' function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

