## These functions are used to create and access a matrix that caches its
## own inverse

## This function creates the caching matrix object.  To actually get the value
## of a cached matrix m, you must access it using m$get().

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setsolve <- function(inverse) i <<- inverse
  getsolve <- function() i
  list( set=set, get=get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## This function gets the inverse of the matrix m

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  i <- m$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setsolve(i)
  i
}
