## A pair of functions that cache the inverse of a matrix rather than compute it repeatedly.
#
#  sample call:
#> x <- matrix (c(1,2,3,4), 2, 2)
#> cm <- makeCacheMatrix (x)
#> cachesolve (cm)
#> cachesolve (cm)
#

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inverse <- function(solve) m <<- solve
  get.inverse <- function() m
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated (and 
##  the matrix has not changed), then the cachesolve should retrieve the 
##  inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get.inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set.inverse(m)
  m
}
