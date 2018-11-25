## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ive <- NULL
    set <- function(y) {
        x <<- y
        ive <<- NULL
    }
    get <- function() x
    setInverse <- function() ive <<- solve(x) #calculate the inverse
    getInverse <- function() ive
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    ive <- x$getInverse()
    if (!is.null(ive)) {
        message("getting cached data")
        return(ive)
    }
    mai <- x$get()
    ive <- solve(mai, ...)
    x$setInverse(ive)
    ive
        ## Return a matrix that is the inverse of 'x'
}
