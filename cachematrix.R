## This has 2 functions that perform the following actions
##(i) takes the cache of the matrix(ii)computes the inverse of the matrix.

## The function "makeCacheMatrix" also creats a special 'matrix' object that caches its inverse.

makeCacheMatrix <- function(mtrx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtrx <<- x;
    mtrx_inverse <<- NULL;
  }
  get <- function() return(mtrx);
  setinv <- function(inv) mtrx_inverse <<- inv;
  getinv <- function() return(mtrx_inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This "cacheSolve' function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` function from above. If the function 
## detects that an inverse has already been calculated x, then
## `cacheSolve` retrives the inverse from the cache. The function detemines if the inverse
##has been calculated by checking to see if the matrix has changed.

cacheSolve <- function(mtrx, ...) {
  mtrx_inverse <- mtrx$getinv()
  if(!is.null(mtrx_inverse)) {
    message("Getting cached data...")
    return(mtrx_inverse)
  }
  data <- mtrx$get()
  invserse <- solve(data, ...)
  mtrx$setinv(mtrx_inverse)
  return(mtrx_inverse)
}
