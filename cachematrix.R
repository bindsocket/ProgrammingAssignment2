## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
