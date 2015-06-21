## The makeCacheMatrix caches a matrix and its inverse in a list.
## The cacheSolve function is used to call the list and return
## the inverse of the matrix using the cached value if available.

## The makeCacheMatrix function takes an input of a matrix
## and returns a list of functions for operations on that
## matrix. The list includes set (sets the matrix values), 
## get (returns the matrix), setinv (calculates and stores
## the inverse) and getinv (returns the inverse)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function takes an input of a list returned
## from the makeCacheMatrix function and returns the inverse
## of the original matrix. If the inverse has been previously
## calculated it uses the cached solution, if not it solves
## for the invere.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("get cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}

