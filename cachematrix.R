## Creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a special "matrix", which is really a
## list containing a function to
##    * set the value of the matrix
##    * get the value of the matrix
##    * set the value of the inverse
##    * get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL    # inverse stored here
    set <- function(y) {
      x <<- y    # store the matrix
      m <<- NULL # initialize inverse to NULL
    }
    get <- function() x # return the matrix
    setinverse <- function(inverse) m <<- inverse # sets the cached inverse
    getinverse <- function() m                    # gets the cached inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) #lists the functions available to makeCacheMatrix
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the inverse, if not NULL, returns inverse, else computes inverse & returns
  m <- x$getinverse() 
  if(!is.null(m)) {
    message("getting cached data") # prints "getting cached data" if inverse exists
    return(m)
  }
  data <- x$get()
  # if X is a square invertible matrix, then solve(X) returns its inverse.
  m <- solve(data, ...)
  x$setinverse(m)
  m
}