## Inversing a matrix is a costly operation.
## So it may be advantageous to read the matrix from the cache if its been already computed 
## rather computing every time.
## Below functions have been designed to achieve this.


## Function MakecahceMatrix()
## Takes a matrix as an input and creates a "special Matrix" of type list
## Special Matrix list holds the following elements
## 1) set the value of the matrix $setmatrix
## 2) get the value of the matrix $getmatrix
## 3) get the inverse of matrix $getinvmatrix
## 4) set the value of inverse of matrix $setinvmatrix

## Input for function <- matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(setmatrix = set, getmatrix = get,
       setinvmatrix = setinverse,
       getinvmatrix = getinverse)
  
}


## Function cacheSolve() 
## This function returns the inverse of a matrix. It uses the solve method to 
## create the inverse of matrix. 
## It first checks if the inverse has already been computed. 
## If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## Important - This function assumes that the matrix is always invertible.
## Input -- Special Matrix created from MakecahceMatrix



cacheSolve <- function(x, ...) {
        
  m <- x$getinvmatrix()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$getmatrix()
  
  m <- solve(data, ...)
  
  x$setinvmatrix(m)
  m
  
}
