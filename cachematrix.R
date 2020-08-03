## The following functions realize caching of the inverse of a matrix


## makeCacheMatrix() creates a wrapper (as a list) of a matrix that could be
## inputted to cacheSolve() to get the potentially cached inverse of it.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <-function() x
  setinverse <- function(y) inverse <<- y
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve() returns the inverse of the matrix wrapped by makeCacheMatrix().
## The returned value will be a cached one if the matrix was called with it 
## before

cacheSolve <- function(x, ...) {
  inverse = x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inverse <- solve(x$get(), ...)
  x$setinverse(inverse)
  inverse
}
