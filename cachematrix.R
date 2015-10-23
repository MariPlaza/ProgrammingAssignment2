# OVERALL SCOPE

## This script presents two functions: makeCacheMatrix and cacheSolve. 
## The purpose of both functions is to provide an efficient computational way to calculate the inverse of a matrix. 
## The function makeCacheMatrix (among other functionalities) stores in cache the result of the inverse of a matrix, until the matrix is changed.
##  While the cacheSolve function checks if the solution is in cache (through get inverse in makeCacheMatrix) and if it is in cache, retrieves the inverse of the matrix if not it calculates the inverse of the matrix and stores it in cache (through setinverse in makeCacheMatrix) 

# MAKECACHEMATRIX COMMENTS

## This is the main function, this function set the matrix in cached and allow to store the inverse of this matrix resulted if this was calculated before. 
## This function is a subset of 4 other functions: set, get, setinverse, getinverse. 
    # SET: Allow to change the matrix storage in the memory and set the inv of this matrix to null. 
    # GET: Return the value of the matrix store in the memory / cache. 
    # SETINVERSE: Set the inverse of the matrix to the variable inv when it is calculated. This procedure stores in cache the inverse of the matrix.  
    # GETINVERSE: Return the value of the inverse of the matrix that is stored in cache. 

makeCacheMatrix <- function(x = matrix()) {
  #Initialize variable inv in Null for further process into the function. 
  inv <- NULL
  
  set <- function(y) {
    # Set the matrix to new value. 
    x <<- y
    #Since the matrix change, this instruction set the inverse to null until it is calculated again. 
    #The operator <<- is used in order to set this value in variable inv of the containing environment, to be available for cachesolve function. 
    inv <<- NULL 
  }
  
  get <- function() x
  
  setinverse <- function(invmatrix) inv <<- invmatrix
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# CACHESOLVE COMMENTS

## This function performs the calculation of the inverse of the matrix, but before doing it, check if the result is already stored in cache. 

cacheSolve <- function(x, ...) {

  ## Return the value of the inverse of the matrix that is stored in cache. 
  inv <- x$getinverse()

  ## If this value is not null, provide the message that the value comes from cache and print the inverse of the matrix. 
  ## If the value is not null, the function ends after printing the cached result and does not perform the calculation again. 
  if(!is.null(inv)) {
    message("getting inverse matrix from cached data")
    return(inv)
  }
  
  #Assign to data the value of the matrix store in the memory / cache. 
  data <- x$get()
  
  #This operation calculates effectively the inversion of the matrix. 
  inv <- solve(data, ...)
  
  #This procedure stores in cache the inverse of the matrix resulted from this previous step.  
  x$setinverse(inv)
  message("getting inverse matrix from new calculation")
  return(inv)
}
