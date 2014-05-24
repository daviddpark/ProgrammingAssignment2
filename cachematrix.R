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
  # initialize the inverted matrix variable to NULL
  i <- NULL

  # function to set the matrix data
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  # function to get the matrix data
  get <- function() x

  # function to set the inverted matrix data
  setinverse <- function(inv) i <<- inv

  # function to get the inverted matrix data
  getinverse <- function() i

  # The return result of this function is a list containing named functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Given some cachedInvertatrix list created with above function
## and optional arguments, calculates the inverse of matrix exactly
## once and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()

  # If the inverted matrix data has already been calculated, return it.
  if(!is.null(i)) {
    message("geting cached data")
    return(i)
  }

  # We have not yet calculated it. First get the matrix data.
  mat <- x$get()

  # Now solve the inverse of the matrix data, passing in optional arguments
  # passed in to invocation of cacheSolve(), and store in i
  i <- solve(mat, ...)

  # Make sure we cache the result in our cacheMatrix object!
  x$setinverse(i)

  # Return the freshly solved inverted matrix data!
  i
}
