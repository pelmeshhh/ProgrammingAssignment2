## This function creates a special "matrix" object that can cache its inverse.

#create a function
makeCacheMatrix <- function(x = matrix()) {
  
  # helping element for inverse matrix
  e <- NULL
  
  # organizing the matrix
  d <- function(matrix) {
    x <<- matrix
    e <<- NULL
  }
  # have matrics
  f <- function() {
    x}
  
  # organize and have inverse matrics
  inv_d <- function(inverse) e <<- inverse
  inv_f <- function() e
  # Nomenclature of all used elements:
  list(d = d, f = f, inv_d = inv_d, inv_f = inv_f)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  x1 <- x$inv_f()
  
  # return inverse matrics if it has been organized already:
  if (!is.null(x1)) {
    message("get previous data")
    return(x1)
  }
  
  # get the matrix: 
  y <- x$f()
  
  ## multiplication process can be implemented by:
  x1 <- solve(y) %*% y
  
  ## invers matrics with the help of first function:
  x$inv_d(x1)
  
  # Return the matrix
  x1
  
}

