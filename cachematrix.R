## makeCacheMatrix creates a function that can cache the 
## matrix and the inverse.
## Function contain a list the a set and get for both the matrix 
## and inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize the inverse matrix value
    i <- NULL
    
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get the value of the martix
    get <- function() {
        x
    }
    
    ## set the value for the inverse matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## get the value for the inverse matrix
    getInverse <- function() {
        i
    }
    
    ## return a list of all get and set functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## The cacheSolve function calculates the inverse of a matrix. 
## A check is made to see if the inverse has already been calculated. 
## If the inverse is already calculated the cached copy is used and the
## computation is skipped. 
## Otherwise, the solve function is used to calculate the inverse of the
## matrix and a stored value is set in the cache.

cacheSolve <- function(x, ...) {
    
    ## get the inverse of x
    i <- x$getInverse()
    
    ## check that the returned inverse for x is not null
    if(!is.null(i)) {
        message("getting cached data")
        
        ## if not null return the cached inverse and exit
        return(i)
    }
    
    ## inverse does not exist
    
    ## get the matrix 
    i <- x$get()
    
    ## calculate the inverse with solve
    m <- solve(x, ...)
    
    ## set the inverse for future use
    x$setInverse(i)
    
    ## return the calculated inverse end exit
    i
    
}
