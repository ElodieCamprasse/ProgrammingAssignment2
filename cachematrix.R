# Here is a pair of functions that cache the inverse of a matrix.
# To do this, the functions create a matrix, cache its inverse and use the cache to retrieve the values that have already been calculated instead of re-calculating them. If they have not been calculated, then they will be during the second step (second function).


# makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) cache <<- solve
  getMatrix <- function() cache
  matrix(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

#cacheSolve is a function that computes the inverse of the special "matrix" returned by the function makeCacheMatrix previously defined. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  cache <- x$getMatrix()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setMatrix(cache)
  cache
}