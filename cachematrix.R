## These functions combined will allow you to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## m will be the cached inverse, set it to NULL to start
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    ## setinverse is called to 'cache' the computed inverse
    setinverse <- function(inverse) m <<- inverse
    
    ## rerturn the cached inverse stored in m (will be NULL if not cached)
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## first check to see if we have a cached version
    m <- x$getinverse()
   
    #¢ if we do, display a message and return the cached inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## otherwise get the original matrix, and calculate the inverse 
    data <- x$get()
    m <- solve(data, ...)
    
    ## cache the inverse
    x$setinverse(m)
    
    ## return the computed inverse
    m    
}
