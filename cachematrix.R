## This script allows us to compute and cache the inverse of matrix x and return it
## without having to inverse matrix x each time we want to get it.



######################################################################################

#############             Function makeCacheMatrix                ####################

######################################################################################


## makeCacheMatrix returns a list of functions applied to the matrix x
makeCacheMatrix <- function(x = matrix()) {
  
  ## invMat will store the inverse of matrix x or the value "NULL" if nothing is stored yet
  invMat <- NULL
  
  ## The function "set" replaces the matrix x by the matrix y and set the inverse matrix to "NULL".
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  
  ## The function "get" returns the matrix x
  get <- function() { 
    x
  }
  
  ## The function "setInverse" stores the inverse of matrix x in the variable invMat
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


## The function "cacheSolve" returns the inverse of a "special" matrix x, 
## created by the function "makeCacheMatrix"
cacheSolve <- function(x, ...) {
  ## We store the value returned by the function "getInverse" in the variable invMat
  invMat <- x$getInverse()
  
  ## If the variable invMat is not empty then it means that the inverse of matrix x
  ## is already in cache and we can use it without any new computation.
  
  if(!is.null(invMat)) {
    ## If we have a matrix in cache then we print a message and return it
    message("Getting cached data")
    return(invMat)
  }
  
  ## If the variable invMat is empty then we have to compute the inverse of matrix x
  ## we begin by storing the matrix in the variable "data"
  data <- x$get()
  
  ## We compute the inverse of this matrix and store it in the variable invMat
  invMat <- solve(data, ...)
  
  ## We set the inverse of matrix X to invMat using the function "setInverse"
  x$setInverse(invMat)
  
  ## And we return invMat.
  invMat
}
