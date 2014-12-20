## Put comments here that give an overall description of what your
## functions do

## The functions create a CacheMatrix object from a matrix that can hold 
## the inverse of the matrix once computed. Any other call to compute the 
## inverse will return this value from the cache

## Write a short comment describing this function
## Creates a CacheMatrix object from the input matrix that has functions for
## 1. getting / setting the matrix
## 2. getting / setting the matrix inverse
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


## Write a short comment describing this function
## Computes the inverse of a CacheMatrix, retrieving it from the cache
## if it has already been computed
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  fromCache = FALSE
  inv <- x$getinv()
  if(!is.null(inv)) {
    fromCache = TRUE
  } else {
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinv(inv)
  }
  list(inv = inv, fromCache = fromCache)
}

## Test function for CacheMatrix
testCacheMatrix <- function() {
  # Generate a random 10x10 matrix
  mat <- matrix(rnorm(10*10,mean=0,sd=1), 10, 10)
  # Convert it to a CacheMatrix
  cmat <- makeCacheMatrix(mat)
  # Calculate the inverse
  result <- cacheSolve(cmat)
  # The first time, the function will calculate the inverse
  message('Call 1: Inverse returned from cache: ', (result$fromCache == TRUE))
  # Do it again
  result <- cacheSolve(cmat)
  # Now, this should be returned from the cache
  message('Call 2: Inverse returned from cache: ', (result$fromCache == TRUE))
}
