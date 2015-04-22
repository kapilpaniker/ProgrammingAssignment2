## Put comments here that give an overall description of what your
## functions do
#The comments are put above the functions

## Write a short comment describing this function
#makeCacheMatrix
#Takes a matrix as input parameter
#creates a list of 4 function objects

makeCacheMatrix <- function(x = matrix()) {
  #check if the input is a matrix, if not, convert it
  if(!is.matrix(x)){
    as.matrix(x)
  }
  #set inverse as Null as it is not yet calculated
  inv <- NULL
  #To set the matrix to new value
  set <- function(y) {
    if(!is.matrix(y)){
      as.matrix(y)
    }
    x <<- y
    #set inverse flag to Null as it needs to be recomputed now
    inv <<- NULL
  }
  #function to get the matrix
  get <- function() x
  #function to set inverse to external value
  setinverse <- function(inverse) inv <<- inverse
  #function to get inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#cacheSolve
#input is object that points to the list of functions created by
#makeCacheMatrix
#returns the inverse after computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #get the matrix
  data <- x$get()
  #calculate the inverse
  inv <- solve(data)
  #set the calculated inverse in the object
  x$setinverse(inv)
  inv
  
}
