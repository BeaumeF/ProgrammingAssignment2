## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     ## solve(c)
     
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setreverse <- function(solve) m <<- solve
     getreverse <- function() m
     list(set = set, get = get,
          setreverse = setreverse,
          getreverse = getreverse)
}

## Write a short comment describing this function
## This is to check if the makeCacheMatrix is already in the cache

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getreverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setreverse(m)
     m
}