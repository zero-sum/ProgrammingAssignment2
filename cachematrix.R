## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatrix
##
## Given an invertible input matrix, returns an object capable of caching the 
## result of computing its inverse via the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  # creates an object containing the input matrix x, and a lexically-scoped
  # cache, and getter/setter functions.
  s = NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

## cacheSolve
##
## When provided invertible matrices wrapped in objects created by 
## makeCacheMatrix, returns the inverse of the matrix.  If the input object 
## already contains a cached version of the result, the cached value is
## returned. Otherwise the inverse is calculated, stored in the cache, and then
## returned to the caller.
cacheSolve <- function(x, ...) {
  # If the input object's cache contains a value, return it.  Otherwise,
  # calculate the inverse, store in the cache and then return the new value.
  s <- x$getinv()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}