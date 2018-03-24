## These functions invert an input matrix; store the inversion in cache-memory;
## and look for the invert solution in cache before proceeding to invert the matrix again
## Thus, this consumes less CPU time, making the function run several times faster

## For the purpose of current assignment, we assume that the input matrix
## is invertible, and do not check for errors on that front

## Function makeCacheMatrix converts an input matrix to a list of functions
## that set the matrix,get a matrix, set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse=NULL) i <<- inverse
  getinverse <- function() i
  ##  list(set=set(),get=get(),setinverse=setinverse(),getinverse=getinverse())
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function cacheSolve takes the list from makeCachematrix as an input
## It checks if the inverse is available in cache
## Prints from the cache memory if the inverse is already calculated, else 
## inverts it, and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
