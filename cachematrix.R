## The cachematrix contains two functions, makeCachematrix() and CacheSolve()
## The functions illustrate caching of an inverse from a matrix. The overall objective
## is to demonstrate the concept of lexical scoping.

## makeCacheMatrix() function creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=matrix) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolv() function requires an argument that is returned by makeCacheMatrix in order to
## retreive the inverse from the cached value that is stored in the makeCacheMatrix objects 
##  environment. 
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

