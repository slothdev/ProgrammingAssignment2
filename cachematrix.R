## Two utility functions to cache and compute/retrieve the inverse of a matrix
##
## makeCacheMatrix  
## This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve 
## This function computes/retrieves the inverse of the special "matrix" 
## returned by the function makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

## makeCacheMatrix()
## set - sets the value of the matrix
## get - gets the value of the matrix
## setinv - sets the inverse of the matrix
## getinv - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve(x, ...)
## Checks for a cached inverse matrix or compute one if not already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached matrix")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

