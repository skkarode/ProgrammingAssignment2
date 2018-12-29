## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates "special matrix" which is a list of 4 functions 
## get(), set() to get and set the value of the matrix respectively
## getInverse() and setInverse() to get and set value of the matrix Inverse
makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function(y) {
      m <<- y
      mInv <<- NULL
    }
    get <- function() m
    setInverse <- function(xInv) mInv <<- xInv
    getInverse <- function() mInv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m  
}
