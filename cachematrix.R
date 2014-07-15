## Put comments here that give an overall description of what your
## functions do


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse and cache its result
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	sinv <- NULL
	set <- function(y) {
		x <<- y
		sinv <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) sinv <<- solve
	getsolve <- function() sinv
	#Return the matrix object with new methods
	list(set = set, get = get,
	   setsolve = setsolve,
	   getsolve = getsolve)

}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  sinverse <- x$getsolve()
  #Check for cached, if not exist inverse the matrix
  if(is.null(sinverse)) {
      #Inverse the matrix if there is no cache
	  data <- x$get()
	  sinverse <- solve(data, ...)
	  #Cache the result
	  x$setsolve(sinverse)
	  sinverse
  }
  else
  {
  	  #return the cache data
  	  message("getting cached data")
	  return(sinverse)
  }
    
}
