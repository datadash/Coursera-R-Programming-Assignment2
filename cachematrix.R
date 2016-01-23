## 
## Functions that support caching the inverse of a Matrix.
## 
## Example Usage:
##    m <- makeCacheMatrix (matrix(1:4, 2, 2))
##    cacheSolve(m)
##    cacheSolve(m)  # cached inverse will be returned


# Creates a matrix wrapper that is capable of caching the
# result of the inverse operation
# 
makeCacheMatrix <- function(x = matrix()) {
  # inverse of the underlying matrix
  inv <- NULL
  # set and cache the underlying matrix, clear cached value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # returns the underlying matrix
  get <- function() x
  
  # set and cache the given inverse
  setInverse <- function(inv) inv <<- inv
  
  # returns the cached inverse
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 
# Computes the inverse of a matrix. If the given maxtrix is a wrapper instance,
# return the cached value if it exists; otherwise compute, cache and return the
# result
#
cacheSolve <- function(x, ...) {
  if ("getInverse" %in% names(x)) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
  } else {
    inv <- solve(x)
  }
  inv
}
