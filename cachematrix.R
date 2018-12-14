## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
makeCacheMatrix <- function(x = matrix()) {
        rv = NULL
        set <- function(y) {
                x <<- y
                rv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) rv <<- inverse
        getInverse <- function() rv
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        rv <- x$getInverse()
        if(!is.null(rv)) {
                message("getting cached data")
                return(rv)
        }
        data <- x$get()
        rv <- solve(data, ...)
        x$setInverse(rv)
        rv
}
