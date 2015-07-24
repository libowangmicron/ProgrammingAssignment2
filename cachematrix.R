## This function computes the inverse of a square matrix and 
## if the inverse of a matrix has been calculated once, it will be
## automatically cached next time when the inversed is asked

## This function will create a special "matrix" objectthat can cache its inverse, 
## this object is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## The following function calculates the inverse of the special "matrix" created with the above function,
## however, it first checks to see if the inverse has already been calculated, if so, it get the inverse
## from the cache and skips the computation. Otherwise, it calculats the inverse of the data and sets
## the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    
    message("getting cached data")
    
    return(inv)
    
  }
  
  data <- x$get()
  
  inv <- solve(data, )
  
  x$setinv(inv)
  
  inv
  
}
