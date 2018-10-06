#library(matlib)


#' Create a custom matrix struct/"object".
#' source: Adapted from R Programming Course John Hopkins
#' 
#' The matrix object is a list containing: 
#' a) set the value of the vector
#' b) get the value of the vector
#' c) get the value of the mean (inverse)
#' d) get the value of the mean (inverse)
#' 
#' @param x A matrix to process.
#' @return the special matrix "object"
#' @examples
#' makeCacheMatrix(matrix(rnorm(25),nrow = 3,ncol = 2))
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set_m <- function(y) {
    x <<- y
    m <<- NULL
  }
  get_m <- function() x 
  setinverse_m <- function(solve) m <<- solve
  getinverse_m <- function() m
  list(set_m = set_m, get_m = get_m,
       setinverse_m = setinverse_m,
       getinverse_m = getinverse_m)
}


#' Calculates the inverse of a matrix.
#' If existing a perviously cached computaion is used.
#' Please Note: Only non-singular matrices have an inverse.
#' 
#' source: Adapted from R Programming Course John Hopkins
#' 
#' @param x A matrix struct as returned by makeCacheMatrix
#' @return the special matrix "object"
#' @examples
#' cacheSolve(makeCacheMatrix(x))
cacheSolve <- function(x, ...) {
  # Returns the inverse
  m <- x$getinverse_m()
  if(!is.null(m)) { # if cached data available
    message("getting cached data!")
    return(m)
  }
  data <- x$get_m()
  m <- solve(data, ...)
  x$setinverse_m(m)
  m
}


#### test functions
a_random_matrix <- matrix(c(1,2,3,9,5,6,7,8,9),nrow = 3,ncol = 3)
#inv_test <- inv(a_random_matrix) # inverse calc by R lib 
a <- makeCacheMatrix(a_random_matrix)
a$get_m() # get orig matrix
a$getinverse_m() # NULL because mothing is saved
cacheSolve(a) # calculate inverse
a$getinverse()  # this is only to show you that the mean has been stored and does not affect anything
cacheSolve(a)
