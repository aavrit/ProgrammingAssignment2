## The following functions are used to calculate and store the inverse
## of a matrix in cache. When the inverse has to be calculated for 
## the same data again, the value of the inverse is retrieved from 
## the cache instead of calculating it all over again.

## The following function takes a matrix as an argument and create 
## a list of 4 functions that can set the value, get the value, 
## set the inverse, and get the inverse for the matrix passed as the argument

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function takes a matrix as an argument
## and checks if its inverse has already been calculated
## If calculated, the inverse is retrieved from cache
## else, the inverse if calculated and stored in the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting Cached Inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  ## Return a matrix that is the inverse of 'x'
}
