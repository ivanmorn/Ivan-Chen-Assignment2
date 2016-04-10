## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The first function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversedMatrix <- NULL
  set <- function(y){
    x<<-y
    inversedMatrix<<-NULL
  }
  get <- function() x
  setinv <- function(inverse) inversedMatrix <<-inverse
  getinv <- function() inversedMatrix
  list(set=set, get=get,
       setinv = setinv,
       getinv = getinv)
}

## The second function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversedMatrix <- x$getinv()
  if(!is.null(inversedMatrix)) {
    message("getting cached data")
    return(inversedMatrix)
  }
  data <- x$get()
  inversedMatrix <- solve(data, ...)
  x$setinv(inversedMatrix)
  inversedMatrix
}