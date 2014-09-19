## The function is used to calculate the inverse of a matrix and cache
## the value for later use.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
        
    set <- function(y) {
      if(nrow(y) != ncol(y)) {
        message("Square matrix is required for properly working.")
      }
      x <<- y
      inverse <<- NULL
    }
    
    get <- function() x
    setmatrix <- function(solve) inverse <<- solve
    getmatrix <- function() inverse
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  inverse <- x$getmatrix()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setmatrix(inverse)
  inverse
}
