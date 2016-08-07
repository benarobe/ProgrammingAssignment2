## These functions work together to create a special object containing 
## a matrix as well as the matrix's inverse so that after the inverse
## is originally calculated once, the result will be available if needed
## again.

## This function creates the matrix object along with some functions
## for getting and setting matrix values and inverse. The function returns
## these functions in a list.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse of the matrix. It returns the stored
## inverse value if available, and calculates the inverse if it had
## not been stored.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}

