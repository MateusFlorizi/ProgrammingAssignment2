## This program gets a matrix, inverts it and return an inverted matrix.
## It caches the resulting matrix to avoid it recalculates again in order to save computation time
## It takes advantage of lexical scoping in order to avoid rerunning the calculation if matrix
## has not changed since last run

## This funciton takes the matrix and return a list with the necessary methods

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(InvMatrix) iv <<- InvMatrix
  getInvMatrix <- function() iv
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## This function uses the methods created in the makeCacheMatrix and calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getInvMatrix()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setInvMatrix(iv)
  iv
}
