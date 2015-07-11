## This script allows us to cache the inverse of matrix x and then reuse the inverse of matrix x without having to reinverse the matrix x again.

## makeCacheMatrix returns a list of function applied to the matrix x which is given as an argument.
makeCacheMatrix <- function(x = matrix()) {
  
  ## invMat will store our cached inverse matrix (or NULL if nothing is stored yet)
  invMat <- NULL
  
  ## The function "set" replaces the matrix x by the matrix y and set the inverse matrix to NULL.
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  
  ## The function "get" returns the matrix x
  get <- function() { 
    x
  }
  
  ## The function "setInverse" caches the inverse of our matrix x, in invMat
  setInverse <- function(solve) {
    invMat <<- solve
  }
  
  ## The function "getInverse" returns the inverse of matrix x: invMat
  getInverse <- function() {
    invMat
  }
  
  ## This list contains the functions defined earlier and is returned by the function makeCacheMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

######################################################################################

#############                    Function cashSolve               ####################

######################################################################################


## The function "cacheSolve" returns the inverse of a special matrix x, 
## created by the function "makeCacheMatrix"
cacheSolve <- function(x, ...) {
  ## We store the value returned by the function "getInverse" in invMat
  invMat <- x$getInverse()
  
  ## If the variable invMat is not empty then it means that the inverse of matrix x
  ## is cached and we can use it without any new computation.
  
  if(!is.null(invMat)) {
    ## If we use the cached matrix then we print a message and return it
    message("getting cached data")
    return(invMat)
  }
  
  ## If then variable invMat is empty then we have to compute the inverse of matrix x
  ## Thus we begin to store the matrix in the variable "data"
  data <- x$get()
  
  ## We compute the inverse of this matrix and store it in the variable invMat
  invMat <- solve(data, ...)
  ## We set the inverse of matrix X to invMat using the function "setInverse"
  x$setInverse(invMat)
  ## And we return invMat.
  invMat
}
