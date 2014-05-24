## These functions create a matrix object and compute it's inverse (assuming
## the matrix is invertible).

## This function creates a matrix object and returns a list of functions to set 
## the value of the matrix, get the value of the matrix, set the value of the 
## inverse of the matrix and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    ## These functions (set, get, setinverse, getinverse are returned as a 
    ## list by makeCacheMatrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    } 
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function checks to see if a value for the inverse of a matrix
## is stored in the cache. It returns the cached value (if it exists)
## or computes the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
    x$setinverse(m)
    m
}

