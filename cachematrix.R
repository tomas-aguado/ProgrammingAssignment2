

## This function builds a cache enabled matrix from a user provided R matrix.
## This is the only object the function cacheSolve can use to invert (or recover the cached inverse)

## Please bear in mind that the call should be done using the same object; example:
  
##   B <- matrix(c(2,5,1,3), 2, 2)
##   CB <- makeCacheMatrix(B)
##   > cacheSolve(CB)
##          [,1] [,2]
##   [1,]    3   -1
##   [2,]   -5    2

##If we make the call again
##   > cacheSolve(CB)
##   getting cached inverse
##         [,1] [,2]
##   [1,]    3   -1
##   [2,]   -5    2

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This  function checks if a inverse war already calculated, and if so it returns the cached inverse
## If no cached inverse is found, the function calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
