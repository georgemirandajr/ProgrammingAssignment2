## These functions can be used to create a special object that  
## stores the inverse of a specified square matrix using makeCacheMatrix. 
## 'MASS' package is required to calculate the inverse.

## the first function below creates a list of functions that can store 
## the inverse of a matrix, 'x'.    

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(ginv) m <<- ginv
  getinverse <- function() m 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## the function below computes the inverse of the matrix returned by
## makeCacheMatrix. It also checks to see if the inverse is already calculated
## and returns the inverse from cache if true.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinverse(m)
  m  ## Returns a matrix that is the inverse of 'x'
}