##
## makeCacheMatrix()
##          Creates an "special" matrix that contains a list of functions to:
##          - set the value of the matrix
##          - get the value of the matrix
##          - set the value of the matrix's inverse
##          - get the value of the matrix's inverse
##
## cacheSolve()
##          Computes the inverse of the passed in matrix (using R's solve() function),
##          previously created with the makeCacheMatrix() function. It checks to see 
##          if the inverse has already been calculate and if so it retrieves the 
##          inverse from the cache and returns it, effectively skipping the computation.

## This functions creates an "special" matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## x is a matrix
    
    ## Return a list of functions to set/get the matrix as well as the inverse to/from
    ## the cache
    
    inv <- NULL
    ## set the matrix and initialize the cache for the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## getter for the matrix "x"
    get <- function() x
    ## set the inverse for matrix "x" in the cache
    setinverse <- function(inverse) inv <<- inverse
    ## get the inverse for matrix "x" from the cache
    getinverse <- function() inv
    ## list of functions to get/set the matrix and cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the "special" matrix; if the inverse has already
## been calculated (and the matrix has not changed), the function returns the inverse from
## the cache.
## The matrix should be an n-by-n matrix
cacheSolve <- function(x, ...) {
    ## x is the "special" matrix created by calling makeCacheMatrix()
    
    ## Return a matrix that is the inverse of 'x'
    
    ## get the matrix's inverse from the cache
    inv <- x$getinverse()
    if (!is.null(inv)) {
        ## we have the inverse in the cache, so return it.
        message("getting cached data")
        return (inv)
    }
    ## get matrix
    theMatrix <- x$get()
    ## compute the inverse
    inv <- solve(theMatrix)
    ## set the inverse in the cache
    x$setinverse(inv)
    ## return the inverse
    inv
}
