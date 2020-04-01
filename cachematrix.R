## Put comments here that give an overall description of what your
## functions do

## makeVector creates a special "vector", which is really a list of functions.

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


## calculates the inverse of the special "vector" created with the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
