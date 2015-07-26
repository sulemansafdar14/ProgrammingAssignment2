# This function creates a special matrix that is in fact a list containing a function to
# 1. Setting the value of the matrix
# 2. Getiing the value of the matrix
# 3. Setting the value of the inverse of the matrix
# 4. Getting the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Set the value of the matrix
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  # Get the value of the matrix
  get <- function() x
  # Set the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  # Get the value of the inverse of the matrix
  getinverse <- function() inv
  # Returns the list of the functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This fuc=nction calculates the inverse of the special matrix created in the function "makeCacheMatrix"
# If the inverse is lready calculated it gets it value from the cache and skips the computation
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  #Checks if value of the inverse is aleady present and if so it retrieves and returns it
  if(!is.null(inv)) {
    message("Getting Cached Data.")
    return(inv)
  }
  # Calculates the inverse of the matrix, if it is not already cached
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  # Returns the inverse value calculated above
  inv
}
