## This is a set of functions that combined to cache potentially time-consuming
## matrix inverse calculations

## This function will create a special matrix which is a list of 4 functions that:
## initialize a matrix
## get the matrix
## set the inversed matrix
## get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function calculates the inverse of the matrix given from the above function
## It first checks if the inversed matrix has already been calculated, if so, it gets
## the inversed matrix from the cache and skip the computation; otherwise, it calculates
## the inverse of the matrix and sets the value of the mean in the cache via the setinverse
## function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}