## These functions create a matrix object capable of caching its inverse
## and provide an interface for computing it (if necessary)

## Create a matrix that can cache its inverse
## The function returns a list of getters and setters for the matrix values and
## the inverse

makeCacheMatrix <- function(x = matrix())
{
    inverse <- NULL
    set <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(newinverse) inverse <<- newinverse
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute or return the cached inverse of 'x'
## 'x' should be an object returned by a call to 'makeCacheMatrix'

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv))
    {
        message("Returning cached inverse")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    
    inv
}
