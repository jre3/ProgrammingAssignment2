## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix. It contains a list of 
## functions to perform the following tasks
##  1. Set the value of the matrix
##  2. Get the value of the matrix
##  3. Set the value of the inverse of the matrix
##  4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() { x }
  setinverse <- function(solve) { inverse <<- solve }
  getinverse <- function() { inverse }

  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function calculates the inverse of a matrix created with
## the makeCacheMatrix function. It checks to see if the inverse
## of the matrix has already been calculated and if so, retrieves 
## the inverse from the cache. If not, it calculates the inverse
## of the matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
