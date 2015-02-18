## Put comments here that give an overall description of what your
## functions do

## returns a list with labels to functions that set and get a matrix,
## and set and get its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## returns the inverse of the cache matrix x
## if the inverse is cached, that is returned,
## otherwise the inverse is solved for, put in the cache,
## and returned
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m)
  x$setinv(inv)
  inv
}
