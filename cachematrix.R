## Based on the example of makeVector and cachemean, here follow 2 functions
## that are used to create the special object which stores the matrix and
## caches its inverse. 
## In order to calculate the inverse, the "solve" funnction is used.

## The first function makeCacheMatrix creates this special matrix: it is
## actually a list containing a function to 
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## The second function calculates the inverse of the matrix that is created by
## the makeCacheMatrix.
## First, we get the inverse value - inv (line 41)
## And check if the value is null (line 42)
## If the inv is not null then the value has already been calculated,
## so we print a message and return the cached value without running
## anythng else, (lines 43-44)
## If the inv is null then the inverse has never been calculate before, so
## by using solve we calculate the inverse of the matrix and we set the inverse
## for the next time the cacheSolve is called (lines 46-48)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <-x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
