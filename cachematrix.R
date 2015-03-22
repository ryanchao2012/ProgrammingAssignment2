## Put comments here that give an overall description of what your
## functions do

## These two functions: makeCacheMatrix and cacheSolve, implement matrix cache
## and solve the inverse matrix.
## The matrix and the inverse matrix can be cached by the function: makeCacheMatrix,
## when we need the inverse matrix, the function: cacheSolve, will check 
## whether the solution has been solved already, if so, return the solution 
## directly, or invoke the solve function to calculate the inverse meatix and
## cache the solution again.

## Write a short comment describing this function
## this function: makeCacheMatrix, cache the inverse matrix, 
## and can be regarded as a function set(list) includeing following functions:
## set: assign a new matrix and initiate the inverse by NULL.
## get: return the stored matrix back.
## setInverse: assign the inverse matrix.
## getInverse: return the stored inverse matrix back.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## this function: cacheSolve, check whether the inverse matrix has 
## been solved and return the solution.
## by passing the "makeCacheMatrix object", cacheSolve extracts the inverse matrix
## first, and examines whether it is NULL, if not, print a message and return the 
## solution directly. otherwise, cacheSolve extracts the stored matrix, and solves
## the inverse matrix, then return and cache the solution back.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
