# In order to load this file you have to use 'source("cachematrix.R")' instead
# of load()... I don't know why...

# This function creates a special "matrix" object that can cache 
# its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # check to see that it's a square matrix
  if(nrow(x) != ncol(x)) {
    print("Cannot determine inverse of this matrix: nrows must equal ncols")
  }
  else {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    # this gives the matrix to the cacheSolve function
    get <- function() x
    # the 'inverse' variable should be the one from cacheSolve
    setinverse <- function(inverse) m <<- inverse
    # returns the matrix
    getinverse <- function() m 
    # returns a list as the new matrix class... this probably won't work
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
  }
}


# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
  # grab the inverse matrix from the special matrix class
  m <- x$getinverse()
  # if we already have the matrix stored... just return it
  if(!is.null(m)) {
    print("getting cached matrix...")
    return(m)
  }
  # grab the original matrix
  inverse <- x$get()
  # compute inverse... thank god we don't have to do this manually
  m <- solve(inverse, ...)
  # make the inverse matrix associated with original special matrix
  x$setinverse(m)
  # return and print out m
  m
}
