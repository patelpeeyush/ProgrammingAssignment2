## These functions return inverse of a matrix if it already exists and if it doesn't then calculates the inverse and returns it
##This helps us save calculation time because if the inverse is already calculated then it can be looked up in the cache


## This function takes a inversible matrix as input and returns a list of functions
## set to set matrix, get to get matrix,  setinverse to set inverse of matrix and getinverse to get the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function takes the matrix created in the function above and tries to find out if inverse already exists, if it doesn't then it calculates the inverse, returns it, and sets value of the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
