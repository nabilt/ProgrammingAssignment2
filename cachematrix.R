## Functions to generate, store and cache the inverse of a matrix

## Matrix object with setter and getter functions 
##   to store the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    invisible(list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse))
}


## Function to calculate the inverse of a matrix.
## If the passed in matrix object has cached the inverse
##   during a previous calculation then this value is returned.

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
