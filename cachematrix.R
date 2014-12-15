## Put comments here that give an overall description of what your
## functions do
# a squred matrix x is created as a list with the matrix
# and a cache in order to save its inverse
## Write a short comment describing this function
# if the matrix is not square a message is printed
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  if (nrow(x)!=ncol(x)) {
    message('x must be square: ',
            'rows= ',nrow(x),', columns= ',ncol(x))
    return(NULL)
  }
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) minv <<- solve
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# The inverse of function x is calculed and stored in cache memory
# if the inverse has been calculated, it is getting of the cache
# if the matrix is singular a message of Lacpack is written
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()
    if(!is.null(minv)) {
      message("getting cached data")
      return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    minv
}