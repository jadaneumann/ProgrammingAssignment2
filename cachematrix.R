## This R function is able to cache a potentially time-consuming computation.
##
## If the contents of a vector are not changing, it may make sense to cache the
## value of the inverse so that when we need it again, it can be looked up in the
## cache rather than recomputed.
##
## The first function, makeCacheMatrix creates a special "matrix" object that
## can cache its inverse
##
##
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the
##cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
