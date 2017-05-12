##The following two functions can help cache the inverse of a matrix rather than
##compute it repeatedly.

##Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) m <<- inverse
      getinv <- function() m
      list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


##Conputes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated, then the cachesolve function will 
##retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
