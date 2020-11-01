## functions to cache inverse values for matrices
## and store them in cache


## function to calculate the inverse of a matrix and store in variable

makeCacheMatrix <- function(x = matrix()) {
  matrixinv<- NULL
  set <- function(y) {
    x <<- y
    matrixinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) matrixinv <<- inv
  getinverse <- function() matrixinv
  list(set = set, get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
}

## function that checks if value has been already calculated, if so
## it retrieves the already calculated value, otherwise it calculates
## and stores it in cache variable

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinverse(x)
    if(!is.null(minv)) {
      message("getting cached data")
      return(minv)
    }
    data <- x$get()
    minv <- solve(x)
    x$setinv(minv)
    minv
}
