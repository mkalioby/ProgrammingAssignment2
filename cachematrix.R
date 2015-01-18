##This script caches the inverse of matrix to save time in subsquent calls.

#This function creates a special "vector", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  }

#The following function calculates the mean of the special "vector" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
