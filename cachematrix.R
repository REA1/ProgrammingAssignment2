## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     # initialize the inverse to be not calculated
  set <- function(y) # set the value of the matrix, thus clear the calculated inverse
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x # get the value of the matrix
  setinv <- function(inver) inv <<- inver # set the inverse of the matrix
  getinv <- function(inver) inv  # get the value of the inverse of the matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv) # return the list of functions
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()  # get the inverse of the function
  if(!is.null(inv)) { # if it has already been calculated
    message("getting the cached data") 
    return(inv)   # return the calculated value
  }
  mat <- x$get() # the inverse has not been calculated, hence get the value of the matrix
  inv <- solve(mat, ...) # calculate the inverse of the matrix
  x$setinv(inv)  # set the inverse of the matrix to the calculated result
  inv  ## Return a matrix that is the inverse of 'x'
}
