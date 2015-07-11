# This script allows us to cache the inverse of matrix x and then reuse the inverse of matrix x without having to reinverse the matrix x again.

# makeCacheMatrix returns a list of function applied to the matrix x which is given as an argument.
makeCacheMatrix <- function(x = matrix()) {
  
  # invMat will store our cached inverse matrix (or NULL if nothing is stored yet)
  invMat <- NULL
  
  # The function "set" replaces the matrix x by the matrix y and set the inverse matrix to NULL.
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  
  # The function "get" returns the matrix x
  get <- function() { 
    x
  }
  
  # The function "setInverse" caches the inverse of our matrix x, in invMat
  setInverse <- function(solve) {
    invMat <<- solve
  }
  
  # The function "getInverse" returns the inverse of matrix x: invMat
  getInverse <- function() {
    invMat
  }
  
  # This list contains the functions defined earlier and is returned by the function makeCacheMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function "cacheSolve" returns the inverse of a special matrix created thanks to the function "makeCacheMatrix"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInverse()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setInverse(invMat)
  invMat
}
