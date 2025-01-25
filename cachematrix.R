## This function creates an environment to store a matrix and its inverse. It provides functions to set or get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y        
    inv <<- NULL   
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Checks if the inverse is already cached.If cached, it retrieves the stored inverse. If not cached, it calculates the inverse, stores it in the cache, and then returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()   
  if (!is.null(inv)) {
    message 
    return(inv)
  }
  
  mat <- x$get()         
  inv <- solve(mat, ...)  
  x$setInverse(inv)       
  inv                     
}
