## This functions creates a special "matrix" object and cache its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- solve(x)
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function Computes the inverse of the special "matrix" returned by the above makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}
