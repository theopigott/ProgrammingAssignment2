## Functions to support caching a matrix inverse

## Create "cache-able" matrix from x: returns list containing functions
## to "get" and "set" value of x and its (cached) inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initialise matrix with value
        # This avoids lazy evaluation issues by using argument x
        set(x)
        # Update matrix to y, reset inverse to null (unknown)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Return value of matrix
        get <- function() x
        # Update inverse
        setInverse <- function(inverse) inv <<- inverse
        # Return inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## Find inverse of x using cached value if x hasn't changed since cacheSolve
## was last called. Otherwise, calculate the inverse of x and cache it.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        # If inv not null it's already been calculated so return cached value
        if (!is.null(inv)) return(inv)
        # Otherwise calculate inverse ...
        mat <- x$get()
        inv <- solve(mat, ...)
        # ... cache it ...
        x$setInverse(inv)
        # ... and return it
        inv
}
