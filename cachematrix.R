## This file contains a pair of functions that can be used to create a matrix that is able to cache its
## inverse. The second function computes the inverse of the special matrix returned by the first function.
##  Jeff Wagg - Nov. 18, 2017

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix. If the 
##  inverse has alreadt been calculated then this function retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
