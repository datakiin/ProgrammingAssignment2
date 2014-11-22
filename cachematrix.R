## Programming assignment 2
## Functions to cache the inverse of an invertible matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setSolve <- function(solve) s <<- solve
      getSolve <- function(solve) s
      list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function computes and returns the inverse of the special matrix object
## created by the makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      s <- x$getSolve()
      if (!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setSolve(s)
      s
}


## Test cases
