## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        #rv is the return value
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
## Computes the inverse of the matrix. 
## If the inverse has already been calculated then the cachesolve retrieves the inverse from the cache.
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
