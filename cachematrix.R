## E Rosa
## May 25, 2020


## This R script defines two functions: makeCacheMatrix and cacheSolve
## Using these functions together the inverse of an invertible square matrix 
## can be solved and cached so it can be retrieved from the cache later

## The function makeCacheMatrix creates a special matrix object that can
## cache its inverse. The function returns a list containing the original
## square matrix, the cached inverse, and methods to set a new matrix argument

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        
        x <<- y
        inv <<- NULL
        
    }
    
    get <- function() {x}
    
    setSolve <- function(inverse) {inv <<- inverse}
    
    getSolve <- function() {inv}
    
    list(set = set, get = get, 
         setSolve = setSolve,
         getSolve = getSolve)

}


## The cacheSolve function computes the inverse of the 'matrix' object that
## is returned by the makeCacheMatrix function
## Cached solutions are not recalculated 
## Rather they are retrieved from the cache

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getSolve()
    
    if(!is.null(inv)) {
        
        message('Getting cached data')
        return(inv)
        
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setSolve(inv)
    inv
    
}
