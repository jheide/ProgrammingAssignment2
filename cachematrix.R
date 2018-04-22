## These functions allow for the creation of a matrix
## which caches it's inverser rather than requiring
## recalculation every time.
## These functions are modeled heavily off of the
## makeVector and cachemean functions provided by
## Roger Peng

## This function creates an enhanced matrix that contains
## functions for storing and retrieving the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Set NULL
    s <- NULL
    
    ## A user could change the underlying matrix being stored here
    ## with the set() function. If they do so, then x needs to be changed
    ## to the passed in matrix and s needs to be reset to NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## get() will returned the stored matrix
    get <- function() x
    
    ## setsolution() will allow for storing the solution
    ## passed in by the user (this modifies the value of s)
    setsolution <- function(solution) s <<- solution
    
    ## getsolution() returns the stored value of s
    getsolution <- function() s
    
    ## return the list with these functions created
    ## this list is the "cache" matrix
    list(set = set, get = get,
         setsolution = setsolution,
         getsolution = getsolution)
    
}


## This function uses the enhanced "cache" matrix
## to return the inverse of the matrix.
## If the inverse has already been calculated then it
## is simply looked up and returned. Otherwise, the inverse
## is calculated and stored in the cacheMatrix and then returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolution()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolution(s)
    s
}