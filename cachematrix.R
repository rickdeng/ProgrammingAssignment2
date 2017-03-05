## A pair of functions that cache the inverse of a matrix.
## Example:
## examplematrix <- makeCacheMatrix(matrix(c(3,4,5,6), 2, 2))
## cacheSolve(examplematrix)

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  setinversematrix <- function(inversematrix) im <<- inversematrix
  getinversematrix <- function() im
  list(set = set, get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)
}


## The following function solve the inverse matrix of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinversematrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinversematrix(im)
  im
}
