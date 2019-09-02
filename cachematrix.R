## Matrix inversion can be a timely computation.  Time saving computation can be saved by caching previous
## computations of matrix inversion to be used again -- especially within a loop.  Below are two functions
## that help cache a matrix inversion, return the cached matrix inversion, or calaculate if the cached inverse
## hasn't been calculated.

## An assumption is made that a matrix will always be invertible. 


## makeCacheMatrix: This function creates a special "matrix" object, where the matrix object can cache
## its inverse for faster retrievel.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special object matrix that is returned by
## makeCacheMatrix.  If the inverse has been cached, cacheSolve will retrive the value.  If the inverse
## has not been cached, then it will calculate the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
