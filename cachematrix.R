## Matrix inversion is usually a costly computation and their may 
## be some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly (there are also alternatives to matrix inversion that we will not 
## discuss here). The assignment is to write a pair of functions that cache the 
## inverse of a matrix.


## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInversed <- function(inversed) {
    inv <<- inversed
  }
  getInversed <- function() inv
  list(set = set, get = get, 
       setInv = setInversed, getInv = getInversed)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mtr <- x$get()
  inv <- solve(mtr)
  x$setInv(inv)
  inv
}
