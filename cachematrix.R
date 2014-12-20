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
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv
}
