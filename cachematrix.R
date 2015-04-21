## These functions are used to calculate and store the inverse
## of a matrix in cache, which can be retrieved from the cache 
## if the inverse is re-calculated for the same matrix.

## The following function takes a matrix as an argument and creates 
## a list of 4 functions that -
##  - set the value of the matrix in cache, 
##  - get the value of the matrix from cache, 
##  - set the value inverse matrix in cache, 
##  - and get the value of inverse matrix from cache 

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
## and checks if its inverse is stored in the cache
## If yes, the inverse is retrieved from cache
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
