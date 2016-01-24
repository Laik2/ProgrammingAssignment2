## The following functions compute and cache the inverse of a matrix. It is assumed that the inverse always exists.

## The function makeCacheMatrix() creates an object that stores a matrix and is additionally able to store its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <<- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse 
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## The function cacheSolve() computes the inverse of a matrix which is created by the function makeCacheMatrix() and stores the inverse in the object. 
## It first checks if the inverse has been computed before. If yes, the cached inverse is returned.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}