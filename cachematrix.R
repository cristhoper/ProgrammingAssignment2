## makeCacheMatrix
## create a matrix with with cache values
## input: @x: matrix()

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve
## determine inverse of matrix and save value cache
cacheSolve <- function(x, ...) {
        inverse <- x$getsolve();
        if(is.null(inverse)){
          inverse <- solve(x$get(), ...)
          x$setsolve(inverse)
        }
        return(inverse)
}
