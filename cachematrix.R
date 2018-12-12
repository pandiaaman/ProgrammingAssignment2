## Put comments here that give an overall description of what your
## functions do
##our aim is to write a pair of functions that cache the inverse of a matrix.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinv <- function(inv) mi <<- inv
  getinv <- function() mi
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  mi <- x$getinv()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data)
  x$setinv(mi)
  mi
}
