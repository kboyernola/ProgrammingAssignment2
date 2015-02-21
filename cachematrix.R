## The function makeCacheMatrix() creates an environment that consists of a
## cached copy of the matrix passed to it and functions that are used  
## by the function cacheSolve() to calculate and cache the inverse of 
## the matrix.  If the inverse matrix was cached by a previous call of cacheSolve(),
## the inverted matrix is retrievee from cache rather than calculate it again.
## Four functions are created by makeCacheMatrix():
## 1) set() caches the matrix passed in and initializes the inverse 
## of the matrix to null, assuring it is either calculated or retrieved 
## from cache when cacheSolve() is called.
## The following functions are used by cacheSolve()
## 2) get() retrieves the cached matrix from the environment
## 3) setinverted() calculates the inverse of the matrix and caches the result
## 4) getinverted() retrieves the cached inverse matrix, if it exists
## This function needs to be called with an invertible matrix as the argument
## It returns a list containing the four functions set(), get(), setinverted()
## and getinverted()

makeCacheMatrix <- function(x = matrix()) {
  imatrix <- NULL
  set <- function(y) {
    x <<- y
    imatrix <<- NULL
  }
  get <- function() x
  setinverted <- function(solve) imatrix <<- solve
  getinverted <- function() imatrix
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
  
}


##This function uses the environment (which contains a matrix and the 
## functions set(),get(), setinverted() and getinverted() created by
## the function makeCacheMatrix().
## cacheSole() is called with the list created by makeCacheMatrix()
## It retrieves the cached matrix and calculates the inversion of it or, 
## if it was previously called with the same list, it notifies the user 
## and retrieves the  inverted matrix from cache instead of recalculating it.
cacheSolve <- function(x, ...) {
  # Try to retrieve the inverted matrix from cache
  imatrix <- x$getinverted()
  if(!is.null(imatrix)) {
    # If cached, let user know and return it  
    message("getting cached data")
    return(imatrix)
  }
  #Otherwise, retrieve the matrix cached by makeCacheMatrix()
  data <- x$get()
  #Calculate the inverse of it
  imatrix <- solve(data, ...)
  #Cache it in case this function is called again with the same list
  x$setinverted(imatrix)
  #Display the inverted matrix
  imatrix
}

## To test:
## e_1 <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(e_1) 
## e_2 <- makeCacheMatrix(matrix(rnorm(10^2),10,10))
## cacheSolve(e_2)
## cacheSolve(e_1) 
## cacheSolve(e_2)

