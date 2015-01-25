## First function can store and retreive objects from the parent environment
## Second function returns the inverse of a matrix either by retrieving 
##it from cache in parent or by calculating it (and then caching)

##  this function stores x and m in the parent environment via
##the set function, then has 3 other functions which can retrieve the matrix x,
##cache the inverse of matrix x, and retrieve the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##setinverse caches the inverse matrix to parent environment
  setinverse <- function(solve) m <<- solve
  ## getinverse retrieves m from parent environment
  getinverse <- function() m
  ##this gives each function in the matrix object a name 
  matrix(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##this function returns inverse of a matrix if cached in parent environment
##if not in parent it then computes inverse matrix and caches it

cacheSolve <- function(x, ...) {
  ## checks if inverse of 'x' already calculated
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##calculates inverse matrix if necessary
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
