# Functions for creating a wrapper around a normal R matrix that enables
# caching of the matrix's inverse.

# Create an object that contains a matrix value and a cached matrix inverse.
# If the user reassigns the value of the matrix, then clear the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  myinverse <- NULL
  set <- function(y) {
    x <<- y
    # Updating the value of the matrix -> clear the saved inverse.
    myinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) myinverse <<- inverse
  getinverse <- function() myinverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Return the inverse of the matrix x.  If we have already calculated the
# inverse of x once, then return the cached value.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
