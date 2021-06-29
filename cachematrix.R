## Put comments here that give an overall description of what your
## functions do

## The below function creates a list of functions to create cache matrix
## inverses and interact with them

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## The below function will either call a cached matrix inverse or solve
## a matrix and cache it

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinv(i)
    i
}
