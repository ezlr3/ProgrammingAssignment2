## Two functions, makeCacheMatrix and cacheSolve
## designed to solve for the inverse of a matrix,
## and cache this inverse to prevent the need to calculate
## the inverse of the same matrix multiple times.

## makeCacheMatrix is a function designed to:
## set the value of a matrix 
## get the value of a matrix 
## set and cache the value of the inverse of this matrix
## get the value of the inverse of this matrix
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) {
      x <<- y
      inverseMatrix <<- NULL

  }
  
  get <- function() x
  
  setinvmatrix <- function(solve) inverseMatrix <<- solve
  
  getinvmatrix <- function() inverseMatrix
  
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
      
}


## cacheSolve is a function designed to:
## check if a matrix's inverse has been saved to cache,
## return the cached inverse if exists, OR
## solve the inverse of the matrix
## return the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinvmatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinvmatrix(inverseMatrix)
  inverseMatrix
  
}

