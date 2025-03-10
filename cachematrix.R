## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  
  # Set the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getInverse <- function() inv
  
  # Return list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves the cached inverse instead of recomputing.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()  # Retrieve the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv
}
