## Programming Assignment 2 - written by Kurt Kessler

## The makeCacheMatrix() will create a matrix from a parameter
## and store that matrix in a global variable
makeCacheMatrix <- function(x = as.matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}
## The cacheSolve() will take the inverse of a matrix.  Before it takes the 
## inverse, it will see if the inverse is stored in a global function.  If so,
## it will use that gobal function and not go through the calculation of 
## creating the inverse.  This saves computing time.
cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

