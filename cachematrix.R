##
## This module contains the following functions:
##
## makeCacheMatrix(x) - Builds a list containing the specified matrix and
##   its cached inverse.
##
## cacheSolve(x) - Computes the inverse of the matrix found in "x". If the inverse
##   was previously computed, the cached copy is returned. Otherwise, the inverse
##   is computed and cached in "x".
##

## Returns a list containing the specified matrix and functions for getting and setting
## both the matrix and its inverse. The inverse is NULL initially.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of the matrix contained in "x". If the inverse was previously
## computed, the cached copy is returned. Otherwise, the inverse is computed and stored
## in "x" prior to being returned.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (! is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mtx <- x$get()
  i <- solve(mtx)
  x$setInverse(i)
  i
}
