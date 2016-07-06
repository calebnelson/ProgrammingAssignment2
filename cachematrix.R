## These two functions create and use a special "matrix" list
## that can cache and store its inverse so that it does not
## have to be calculated every time.

## Creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("pulling inverse from cache")
    return(i)
  }
  m <- x$get()
  i <- solve(m)
  x$setinverse(i)
  i
}
