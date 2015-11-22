## Put comments here that give an overall description of what your
## functions do
## Adding a comment to test uploading to github

## Write a short comment describing this function
## These functions are used to cache the inverse of a matrix to avoid repeated calculation
## makeCacheMatrix takes a matrix and returns a list of four functions for the supplied matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) { 
                x <<- y    ## changes the matrix x stored in the main function
                m <<- NULL  ## clears out any old cached inverse from a prior matrix
        }
        get <- function() x ## returns the matrix x stored in the main function
        setinv <- function(inverse) m <<- inverse ## stores the value of  inverse
        getinv <- function() m  ## returns the value of inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve takes the output of makeCacheMatrix on a matrix and either 
##      * returns the store dalue of its inverse, if it has already been cached
##      * or, calculates the inverse, caches it, and also returns that inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        ## retrieves the inverse if it is already cached
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()                 ## assigns data to the value of the input matrix
        m <- solve(data, ...)           ## computes the inverse and assigns it to m
        x$setinv(m)                     ## stores the value of the inverse
        m                               ##returns the inverse
}
