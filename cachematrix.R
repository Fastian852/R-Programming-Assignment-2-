## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a cacheMatrix object as a list of curried functions
## which allow setting/getting a cached value for the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() {
    x
  }
  setcache <- function(x) {
    cache <<- x
  }
  getcache <- function() {
    cache
  }
  list(set=set, get=get, setcache=setcache, getcache=getcache)
}

## cacheSolve returns the cached value of the input if it exists
## otherwise it inverts
cacheSolve <- function(x, ...) {
  m <- x$getcache()
  if(!is.null(m)) {
    message("returning cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}
