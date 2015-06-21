## create the matrix and caching functions
makematrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinvmat <- function(matrixinversion) inv <<- matrixinversion
  getinvmat <- function() inv
  list(setmat = setmat,
       getmat = getmat,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}
## cache the matrix inverse
cacheinvmat <- function(x) {
  inv <- x$getinvmat()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmat()
  inv <- solve(data)
  x$setinvmat(inv)
  inv
}
