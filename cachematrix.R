## CacheMatrix object and associated functions

## This function operates on the CacheMatrix object.  Functions to get and set the matrix and get and set the inverse matrix are included

makeCacheMatrix <- function(x = matrix()) {
    # set inverse to NULL by default
    inverse <- NULL
    # function to set the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # function to get the matrix
    get <- function() x
    # function to set the inverse of the matrix
    setInverse <- function(i) inverse <<- i
    # function to get the inverse of the matrix
    getInverse <- function() inverse
    # return the list of functions available to this object
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function returns the inverse of the CacheMatrix object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # get the inverse
    inverse <- x$getInverse()
    # if it exists, return the cached value
    if(!is.null(inverse)) {
        message("fetching cached data")
        return(inverse)
    }
    # otherwise... we need to calculate the inverse, cache it, and return it
    # get the matrix
    matrix <- x$get()
    # calculate the inverse (solve without 'b' gets the inverse)
    inverse <- solve(matrix)
    # cache the inverse
    x$setInverse(inverse)
    # return the inverse
    inverse
}
