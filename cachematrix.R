## Two functions, makeCacheMatrix and cacheSolve
## designed to solve for the inverse of a matrix,
## and cache this inverse to prevent the need to calculate
## the inverse of the same matrix multiple times.

## makeCacheMatrix is a function designed to:
## set the value of a matrix -- valid and square matrices only.  
## get the value of a matrix 
## set and cache the value of the inverse of this matrix
## get the value of the inverse of this matrix
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) {
    paramClass <- class(y)

    ## Check if parameter passed to the function is a matrix
    if (paramClass=="matrix") {
      paramRows <- nrow(y)
      paramCols <- ncol(y)
      
      ## Check if matrix is a square matrix
      if (paramRows==paramCols) {
        x <<- y
        inverseMatrix <<- NULL

      ## Matrix is not a square matrix - return error message.
      } else {
        print("Please pass a valid square matrix to the function.")
      }
      
    ## Parameter is not of class matrix - return error message. 
    } else {
      print("Please pass a valid matrix to the function.")  
    }
    
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

