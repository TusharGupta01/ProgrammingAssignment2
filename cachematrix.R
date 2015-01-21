# this function works like a class, it creates a list
# that contains 4 member functions: set, get, setInv
# and getInv. it uses <<- assignment operator so that
# these internal variables are not exposed to the
# outside environment. 

makeCacheMatrix <- function(x = matrix()) {
  
  my <- NULL
  set <- function(y) {
    x <<- y
    my <<- NULL # it also initialises xinv to null
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) my <<- inv # set the inversed  matrix
  getInv <- function() my # return the inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  m <- x$getInv() # get the inversed matrix from object x
  # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data) # we solve it
  x$setInv(m) # we then set it to the object
  m # return the solved result
}