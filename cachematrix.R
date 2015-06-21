## Functions to return the inverse of a matrix. The functions can
## cache a computation of the inverse matrix, in order to avoid
## time-consuming computations. 

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse of the matrix x
    inv <- NULL
    
    ## Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Function to get the matrix
    get <- function() x
    
    ## Function to set the inverse of matrix x
    setinverse <- function(inverse) inv <<- inverse
    
    ## Function to get the inverse of matrix x
    getinverse <- function() inv
    
    ## Return a list of the methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Get the inverse of x
    inv <- x$getinverse()
    
    ## Check if the inverse of x has already been calculated
    if(!is.null(inv)) {
        ## If the inverse of x has already been calculated
        ## then return this value.
        
        message("getting cached data")
        
        return(inv)
    }
    
    ## If the inverse of x hasn't been calculated
    ## then compute it
    
    ## Get the matrix
    data <- x$get()
    
    ## Calculate the inverse of matrix
    inv <- solve(data, ...)
    
    ## Set the inverse
    x$setinverse(inv)
    
    ##Return the inverse
    inv
}
