## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

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
