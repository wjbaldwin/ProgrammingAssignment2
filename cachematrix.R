
## Creates a new object that can cache its own inverse. Provides 
## functions to:
##      set the data matrix (which clears the cache)
##      get the data matrix
##      set the cached inverse
##      get the cached inverse (NULL if it hasn't been set)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Returns the inverse of the matrix. It returns the cached 
## value if it already has been calculated, otherwise, it 
## calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
