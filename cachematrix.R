## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The makeCacheMatrix function returns an object that can set and get a matrix and the inverse of that matrix.
# If the user sets the matrix variable, a check is done to see if the matrices are identical and the matrix variable will be set if they aren't.
makeCacheMatrix <- function(x) {
  m <- NULL
  b <- FALSE
  set <- function(y) {
    if(!identical(x,y)){
      b <<- FALSE
    }
    x <<- y
  }
  get <- function() x
  setinverse <- function(n) {
    m <<- n
    b <<- TRUE
  }
  getinverse <- function() m
  getvalidinverse <- function() b
  setvalidinverse <- function(y) {
    b <<- y
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getvalidinverse = getvalidinverse,
       setvalidinverse = setvalidinverse)
}


## Write a short comment describing this function
#The cacheSolve function determines if the inverse has already been calculated.  If it does exist then it is returned.
#The function uses the solve method to find the inverse.
The matrix object has a boolean variable that shows whether the matrix has changed since the last use of the cacheSolve function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(x$getvalidinverse()) {
    #message("getting cached data")
    return(m)
  }else if(!x$getvalidinverse()){
    #message("getting the inverse")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    x$setvalidinverse(TRUE)
  }
  #message("done")
}

