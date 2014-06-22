
# Make a list of matrix with a calculated inversion
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
# Set value of the matrix list  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

# Get the value of the matrix
  get <- function() x

#Set the value of the inversion
  setsolve <- function(solve) m <<- solve

# Get the value of the inversion
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# This calculates the inversion of the matrix list 
cacheSolve <- function(x, ...) {
  # Get the inverson
  m <- x$getsolve()
  
  # If inversion already calculated, get that cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Otherwise calculate the inversion
  data <- x$get()
  m <- solve(data, ...)
  
  # Now set the inversion data onto the cache
  x$setsolve(m)
  m
}