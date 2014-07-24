## makeCacheMatrix() can be called to create an object containing a matrix and its cached inverse.
## cacheSolve() can be called to get the cached inverse for an object created withmakeCacheMatrix()

## Creates an object containing a matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Returns the inverse of x if cached, else computes & caches the inverse before returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

# Test code:
# mymat <- makeCacheMatrix()
# mymat$set(matrix(c(1,2,3,4),nrow=2,ncol=2))
# cacheSolve(mymat)
# cacheSolve(mymat) # run cacheSolve a second time to see the 'getting cached data' message