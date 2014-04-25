## The function makeCacheMatrix creates a special vector which contains
# functions to cache the inverse of a matrix.  The fuction cacheSolve will
# retrieve the inverse of a matrix if it is available or solve it if not.

## makeCacheMatrix is a first class function (passes functions as arguments to
# other functions).  The output of makeCacheMatrix is a list of functions to 
# perform the following:
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the inverse of the matrix
#   4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve will calculate the inverse of a matrix, but first it
# will check to see if the value already exists

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
  ## Returns a matrix that is the inverse of 'x'
}
