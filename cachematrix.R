## Functions to support caching a matrix inverse

## Create "cache-able" matrix from x: returns list containing functions
## to "get" and "set" value of x and its (cached) inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## Find the inverse of x, using the cached inverse if x hasn't
## changed since cacheSolve was last called. Otherwise, 
## calculate the inverse of x and cache it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) return(inv)
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
