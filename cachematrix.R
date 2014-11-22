## Programming assignment 2
## Functions to cache the inverse of an invertible matrix

## This function initializes a special matrix object that can cache its inverse
## and also initializes callable functions for cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
      
      # Initializes the variable s as NULL
      s <- NULL
      
      # Caches value of matrix x and reset s to NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      
      # Returns original matrix x
      get <- function() x
      
      # Caches inverse matrix s for initial access using superassignment
      setSolve <- function(solve) s <<- solve
      
      # Returns s on subsequent access
      getSolve <- function(solve) s
      
      # Lists callable functions to be used by cacheSolve()
      list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function checks if cache contains inverse of the special matrix object
## and computes it if not cached.
cacheSolve <- function(x, ...) {
      
      # Assigns s as cached inverse of x
      s <- x$getSolve()
      
      # Checks and returns cached inverse of matrix x
      if (!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      
      # If not in cache, assigns data as input matrix x
      data <- x$get()
      
      # and uses built-in solve() to compute inverse matrix
      s <- solve(data, ...)
      
      # Assigns computed solution as s
      x$setSolve(s)
      
      # Prints non-cached inverse of matrix x
      s
}
