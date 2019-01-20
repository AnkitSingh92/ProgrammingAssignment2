## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to, 
## set the value of the martix
## get the value of the martix
## set the value of the martix 
## get the value of the martix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the mean has already been calculated
cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(i)
  i
}
