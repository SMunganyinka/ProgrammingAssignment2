## These functions create a special matrix object that can cache its
## inverse. If the inverse has already been computed, it is retrieved
## from the cache instead of being recomputed.

## Creates a special matrix object that can store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() inv
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Returns the inverse of the matrix stored in the special matrix object.
## If the inverse has already been computed, it retrieves it from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}