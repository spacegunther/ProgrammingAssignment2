## Put comments here that give an overall description of what your
## functions do
## Steps to use theses functions : 
## 1) Call makeCacheMatrix to create a cached Matrix
## 2) Pass the returnned object into cacheSolve to get the inverse of that matrix 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Initalisation to Null
  inverse <- NULL
  
  set <- function(y) {
    ## Just to check that the Matrix can be solved (even if this is assumed by the exercice, but who knows? :)
    if(nrow(y) != ncol(y)) {
      message("Number of rows isn't equal number of columns. The solve function will not work properly.")
    }
    ## Assign the new value to parent's x
    x <<- y
    ## Assign NULL to parent's inverseMatrix variable
    inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  ## if the cached inversee matrix exists, return it
  if(!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  ## If the cached inverse matrix does not exist, calculate and return it 
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
