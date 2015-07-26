cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
}
# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# This function:
# 1. sets value of the matrix
# 2. gets value of the matrix
# 3. sets value of the inverse
# 4. gets value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setmean <- function(inverse) inverted <<- inverse
  setInverse <- function(inverse) inverted <<- inverse
  getInverse <- function() inverted
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If inverse has already been calculated and matrix has not changed 
# then the cacheSolve should retrieve the inverse from the cache.
# Function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  inverted <- x$getinverse()
  if(!is.null(inverted)) {
    message("Getting cached data")
    return(inverted)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inverted)
  inv
}
