## This is a solution to programming assignment 2

## makeCacheMatrix will create a special matrix that is capable of cahing its inverse
## x will contain the matrix
## i will contain the inverse matrix (NULL if the inverse has not been calculated yet)
## setInverse will allow the cacheSolve function to set the value of i
## getInverse will return the value of i
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The cacheSolve gets the inverse of the matrix x
## The first invocation will calculate the inverse and save the result
## Subsequent invocations will return the cached result
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  i <- solve(x$get(), ...)
  x$setInverse(i)
  i
}
