## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xinvrs <- NULL 
  set <- function(y) {
	x <<- y
	xinvrs <<- NULL 
  }

  get <- function() x 
  setInvrs <- function(invrs) xinvrs <<- invrs 
  getInvrs <- function() xinvrs 
  list(set = set, get = get,
	   setInvrs = setInvrs,
	   getInvrs = getInvrs)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvrs()
        if(!is.null(m)) { 
        	message("getting cached data")
        	return(m) 
        }
        data <- x$get() 
        m <- solve(data) 
        x$setInvrs(m) 
        m                                                                                             
}
