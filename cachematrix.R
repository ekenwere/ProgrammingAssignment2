## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initialize mCache, the inverted matrix.
  mCache <- NULL

  # Store the inverted matrix.
  storeMatrix = function(stor) {
    x <<- stor
    mCache <<- NULL
  }

  # Produce the original "matrix" on demand.
  retrieveMatrix = function() x 
  
  # Produce the inverted matrix "mCache"
  storeInverse = function(y) mCache <<- y
  
  # Retrieve the inverted matrix "mCache" on demand.
  retrieveInverse = function() mCache
  
  
  # We return a list of functions defined in makeCacheMatrix.  
  list(storeMatrix = storeMatrix, retrieveMatrix = retrieveMatrix, storeInverse = storeInverse,
       retrieveInverse = retrieveInverse)
}

## Write a short comment describing this function

# This miserable function will accept input "x".
cacheSolve <- function(x, ...) {
  
  # See if there's a cached inversion. If there is, you found Nemo!
  localInversion <- x$retrieveInverse()
  if(!is.null(localInversion)) {
    message("Found Nemo.")
    return(localInversion)
  } else {
    message("Finding Nemo.")
    
    # Solve and store the bloody matrix.
    localMatrix <- x$retrieveMatrix()
    localCache <- solve(localMatrix,...)
    x$storeInverse(localCache)
    return(localCache)
  }
}
