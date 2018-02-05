## Function makeCacheMatrix() takes as an input invertible matrix and 
## creates a list of 4 functions: set(), setinv(), get() and getinv(). 
## Function cacheSolve() takes as an input an object of type returned 
## by makeCacheMatrix() and checks if the inverse of earlier entered matrix is 
## cached or not. If yes, it returns cached value, 
## if not - it computes, returns and caches it.

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
