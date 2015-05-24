## Functions makeCacheMatrix and cacheSolve create a special matrix object and calculate the inverse respectively .
##if the inverse has already been calculated for the input matrix , then the 
##value if retrieved from cache without recomputing the value.
##for new matices that are input, the inverse is newly computed and stored in cache

## function makes use of lexical scoping and uses <<- operator to assign values to objects 
## in a different environment than where those were created

makeCacheMatrix <- function(x = matrix()) {
  #pass input matrix to be inverted as argument
  #initialize inverse to null
  inv <- NULL
  #write the set function to set the value of the matrix 
  #and set inverse to null if the input matrix has changed
  set <- function(y) {
  
    x <<- y
    inv <<- NULL
  }
  #get input matrix 
  get <- function() x
  #set inverse value of given matrix
  setinv <- function(inverse) inv <<- inverse 
  #get the value of inverse that is computed
  getinv <- function() inv
  #return a list of four functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  #input from makeCacheMatrix
  inv <- x$getinv()
  #if inverse has been already computed for given matrix then get the value from cache
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  #otherwise find the inverse and store it in cache
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  
  x$setinv(inv)
  #return the inverse as output 
  return(inv)
}

