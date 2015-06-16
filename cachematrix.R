## Created a functions that cache the inverse of a matrix.
## It does caching of the inverse of a matrix

## This function that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mcm_x <- NULL
  set <- function(y) {
    x <<- y
    mcm_x <<- NULL
  }
  get <- function() x
  setiv<- function(inverse) mcm_x <<-inverse
  getiv <- function() mcm_x
  list(set = set, get = get,
       setiv = setiv,
       getiv = getiv)
}


## Create a function that computes the inverse of the object created from the above `makeCacheMatrix` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mcm_x <- x$getiv()
  if (!is.null(mcm_x)) {
    message("getting cached inverse matrix")
    return(mcm_x)
  } else {
    mcm_x <- solve(x$get())
    x$setiv(mcm_x)
    return(mcm_x)
  }
}
