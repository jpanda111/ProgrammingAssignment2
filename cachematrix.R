## this code contains two functions
## first function creates an R object that stores a Matrix and its Inverse
## Second one retrieve the Inverse from the cached value that is stored in the first function object's environment
## The second one has to take an argument that is returned by makeCacheMatrix() in order to retrieve.

## makeCacheMatrix() builds a set of functions and returns the functions within
## a list to the parent environment.
## It contains four functions: set(),get(),setInverse(),getInverse().
## It also includes two data variables, x and theInverse.

makeCacheMatrix <- function(x = matrix()) {
  theInverse <- NULL
  set <- function(y) {
    x <<- y
    theInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) theInverse <<- Inverse
  getInverse <- function() theInverse
  
  ## create list with methods for get / set of both original matrix
  ## and its inverse, return the list to parent environment
  ## note that this technique allows use of $ operator to access
  ## each function from the list
  list( set=set, get=get,
        setInverse=setInverse,
        getInverse=getInverse)
  
}


## cacheSolve() takes the argument from makeCacheMatrix() and retrieve the cached Inverse
## if it's NULL, calculate Inverse and cache it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  theInverse <- x$getInverse()
  if (!is.null(theInverse)) {
    message("getting cached data")
    return(theInverse)
  }
  data <- x$get()
  theInverse <- solve(data, ...)
  x$setInverse(theInverse)
  theInverse
}
