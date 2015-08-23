## makeCacheMatrix sets and gets a matrix as well as
## setting and getting its inverse using solve func.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve checks to see if we have a cached value
## for the inverse matrix and if not it calculates it

cacheSolve <- function(x, ...) {

  ## check for cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## no cache found
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
