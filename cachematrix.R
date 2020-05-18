# This script is used to create a special object that stores a matrix and caches its inverse. 

## Setup makeCacheMatrix as a function, returning the inverse of the input 

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get<- function() x
  setInverse <- function(inverse) i<<- inverse
  getInverse <- function() i
  list(set=set, 
       get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}

##  This function computes the inverse of the "matrix" returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse() 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
