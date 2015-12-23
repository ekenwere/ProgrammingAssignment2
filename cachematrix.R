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

# This miserable function will accept input "x" for some reason.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  localInversion <- x$retrieveInverse()
  if(!is.null(localInversion)) {
    message("Found Nemo.")
#    cat("x:",x,"\nand\ny:",y,"\n")
    return(localInversion)
  } else {
#    makeCacheMatrix(x)
    message("Finding Nemo.")
    
    # Get the bloody matrix.
    localMatrix <- x$retrieveMatrix()
    localCache <- solve(localMatrix,...)
    x$storeInverse(localCache)
    return(localCache)
  }
}
