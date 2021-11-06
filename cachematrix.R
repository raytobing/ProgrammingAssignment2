# The function makeCacheMatrix makes an object that caches the inverse of a matrix,
# such that if the 

makeCacheMatrix <- function(x = matrix()) {
  m   <- NULL
  
  set <-  function(y) {
    x <<- y
    m <<- NULL
  }
  
  get    <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function determines if we have a cache of the inverse of the matrix we use as an input.
# If it's there, then we simply return the cached data. Otherwise, the function calculates the inverse
# and returns it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m    <- solve(data, ...)
  x$setinv(m)
  
  return(m)
}
