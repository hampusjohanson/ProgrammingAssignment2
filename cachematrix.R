## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## set a local m to null
  m <- NULL
  
  ##create funtion store value of x and m, set argument y to x value 
  set <- function(y) {
    
    x <<- y
    
    m <<- NULL
    
  }
  
##function to return the value of x
  
  get <- function() x

  ## set value of m to the inverse argument
  setinverse <- function(inverse) m <<- inverse
  
  ## return the value of m
  getinverse <- function() m
  
  ## list return with all functions as arguments
  list(set = set, get = get,
       
       setinverse = setinverse,
       
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse() ##set m to 
  ## checks if m is null, already calculated
  
  if(!is.null(m)) {
    
    message("gets the cached data")
    
    
    return(m)
    
  }
  ## if m is null, calculation of mean comes here. First off store matrix as dat

  dat <- x$get()
  
  ##store inverse as m
  m <- solve(dat, ...)
  
  
  x$setinverse(m)

##return value of inverse

  m
  
}
