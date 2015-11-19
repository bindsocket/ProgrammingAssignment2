## Put comments here that give an overall description of what your
## functions do
## Basic Usage:
## 1. Create object:
##  myMatrixObj <- makeCacheMatrix()
## 2. Set matrix element:
##  myMatrixObj$set(matrix(data,rows,cols))
## 3. Use cacheSolve to get the inverse. Subsequent calls will pull from
##    the cached value.
##  inv <- cacheSolve(myMatrixObj) 


## Write a short comment describing this function
## Produces an object that had the ability to store a matrix,
## retrieve the matrix, get the inverse matrix, or return
## the cached inverse of the matrix.
## Init: headless init
## Functions Supplied:
##  set(x): sets the matrix element
##  get(): returns the matrix element+
##  setinv(inv): sets the cached inverse matrix
##  getinv(): returns the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL
  set <- function(y) {
     xm <<- y
     inv <<- NULL
  }
  get <- function() {xm}
  setinv <- function(xinv) {inv<<-xinv}
  getinv <- function() {inv}
  list(set = set,get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## function cacheSolve: will when supplied with the object
## created using the makeCacheMatrix function, return the
## inverse of the matrix in said object. If the inverse had
## already been created then a cached value is returned;
## otherwise the inverse is calculated, stored in the cache
## and then returned.
## Input: makeCacheMatrix object
## Returns: Inverse of makeCacheMatrix matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if (!is.null(xinv)) {
    message("retrieving cached value...")
    return(xinv)
  }
  v_data <- x$get()
  v_inv <- solve(v_data)
  x$setinv(v_inv)
  return(v_inv)
}
