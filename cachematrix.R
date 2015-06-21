#
# Sherri Leach
#
# Assignment 2 for Coursera R Programming Course
#

makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix creates an empty matrix object and defines the functions that can be run against the newly created matrix object.
  # These functions include set, get, getinverse,setinverse which is stored in a list object.
  # 
  # Args: 
  # x: defined matrix 
  #
  # Returns:
  # Nothing
  #  

  #Sets local variable "m" to NULL
  m <- NULL

  #Defines the "set" function which accepts the local y matrix and sets it using the superassignment operator to set the variable "m" 
  #in the containing environment (global) to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  #Defines the "get" function which returns the local "x" matrix
  get <- function() x
  
  #Defines the "setinverse" function which calculates inverse matrix and stores it in the variable "m" in the containing environment (global) 
  #to the inverse matrix   and returns the inverse matrix
  setinverse <- function(x) m <<- solve(x)
  
  #Defines the "getinverse" function which returns the inverse matrix if assigned, else NULL 
  getinverse <- function() m
  
  #Executes the "list" function which defines the set, get, setinverse and getinverse local variables
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Computes the inverse of the special matrix object that was created by running the makeCacheMatrix function.  If the inverse matrix has
  # already been calculated (and the matrix has not been changed),then cacheSolve will retrieve the previously computed inverse matrix that is
  # stored in cache.  If the inverse matrix has not been computed, cacheSolve will call the setinverse function and update the special matrix object.
  # 
  # Args: 
  # x: defined matrix 
  #
  # Returns:
  # m: inverse matrix of x
  #

  #Executes the function "getinverse" of "x" matrix and stores inverse matrix in local variable "m" 
  m <- x$getinverse()
  
  #Tests to see if local variable "m" is not NULL; meaning the inverse matrix has already been assigned
  if(!is.null(m)) {

    #Print message that data requested is already cached 
    message("Displaying previously cached data....")

    #Returns the value of the "getinverse" function stored in local variable "m"
    return(m)
  }

  #Local Variable "m" has not been assigned to a calculated inverse matrix and therefore is equal to NULL
  message("Calculating inverse of matrix....")

  #Retrieve the matrix
  data <- x$get()

  #Calculate the inverse matrix and store in local variable "m" and assign the inverse matrix to the cache list 
  m<-x$setinverse(data)

  #Return the value of the setinverse function stored in local variable "m"
  return (m)
}

