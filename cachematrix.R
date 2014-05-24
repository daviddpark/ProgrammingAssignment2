## Expose two functions that cache matrix inversion
## Considering the expense on a large matrix, if we
## attempt to get the inverse repeatedly, it makes 
## more sense to memoize it and reuse rather than
## recompute.

## Create a list of functions to manage the caching
## of the inverse calculation of a matrix
## Use like this:
##
## > cm <- makeCacheMatrix(some_matrix)
## > cm$set(some_matrix) sets the matrix data
## > cm$get() returns the matrix data, NULL until set
## > cm$setinverse(inverted_matrix) sets inverse
## > cm$getinverse() returns inverse, NULL until set

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Given some cachedInvertatrix list created with above function
## and optional arguments, calculates the inverse of matrix exactly
## once and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("geting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinverse(i)
  i
}
