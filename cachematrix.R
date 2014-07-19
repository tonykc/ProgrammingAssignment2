## cachematrix.R
## 
## Pair of functions "makeCacheMatrix" & "cacheSolve" that cache 
## the inverse of a matrix rather than compute it repeatedly.
##
## Sample test:
##   > a <- matrix (c(1,2,3,4), 2, 2)
##   > cm <- makeCacheMatrix (a)
##   > cacheSolve (cm) ## solve inverse for the 1st call
##   [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
##   > cacheSolve (cm) ## get inverse from cache
##   getting cached data
##   [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
##   > cm$set (matrix (c(1,2,3,4), 2, 2)) ## assign same matrix
##   > cacheSolve (cm) ## should still get from cache
##   getting cached data
##   [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
##   > cm$set (matrix (c(4,3,2,1), 2, 2)) ## modify the matrix
##   > cacheSolve (cm) ## solve again as matrix modified
##   [,1] [,2]
##   [1,] -0.5    1
##   [2,]  1.5   -2

## Function
##   makeCacheMatrix
##
## Objective
##   To create a special "matrix" object that can cache its inverse.
##
## Output
##   Return a list of 4 funcions

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the cache for inverse as NULL
  inverse <- NULL
  
  set <- function(y) {
    ## Stored/Set the matrix, clear the cache of inverse
    ## if y is not identical to x
    if (!identical (x, y)) {
      ## The <<- operator tells R not to create new variable x & inverse
      ## in this context (enviroment).  Instead, look for the variables
      ## in some parent context for the content assignments.
      x <<- y
      inverse <<- NULL      
    }
  }
  
  get <- function() x
  
  set.inverse <- function(solve) inverse <<- solve
  
  get.inverse <- function() inverse
  
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

## Function
##   cacheSolve
##
## Objective
##   This function computes the inverse of the special "matrix" returned by 
##   makeCacheMatrix above. If the inverse has already been calculated (and 
##   the matrix has not changed), then the cachesolve should retrieve the 
##   inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inverse <- x$get.inverse()

  ## If the inverse returned is not null, i.e. already 
  ## calculated, simply return this cached value.

  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  ## Else, do the calculation and "cache" the result.

  data <- x$get()
  inverse <- solve(data, ...)
  x$set.inverse(inverse)
  inverse
}

