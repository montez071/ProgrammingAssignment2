## Here we are using 2 functions to create a special object that stores a matrix
## and caches the inverse of that matrix

## makeCacheMatrix creates a special matrix where cached inverse matrix will be stored

makeCacheMatrix <- function(x=matrix()){
  minverse <- NULL
  set <- function (y) {
    x <<- y
    minverse <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) minverse <<- inverse
  getinverse <- function() minverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## CacheSolve returns the inverse matrix by first looking in the cache before computing

CacheSolve <- function(x, ...) {
  minverse <- x$getinverse()
  if (!isnull(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  m <- x$get()
  minverse <- solve(m, ...)
  x$setinverse(minverse)
  minverse
}
