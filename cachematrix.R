## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse.


# makeCacheMatrix creates a special matrix which is really a list
# containing a function to get/set the value of the matrix and
# to get/set the value of the inverse.

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


# cacheSolve calculates the inverse of the matrix created in the above function.
# It first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from cache and skips the computation.
# Otherwise, it calculates the inverse of the data and caches the value.

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
    # Return a matrix that is the inverse of 'x'
}
