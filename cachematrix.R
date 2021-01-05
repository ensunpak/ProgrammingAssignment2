# This function will take in the inverse of a specified matrix (x) and check
# if it is the same matrix as before. If it is the same, it will return the
# cached inverse of the matrix instead. The function also allows for getting
# and setting of a matrix object.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function will return the inverse of a matrix specified as x
cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("Getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}