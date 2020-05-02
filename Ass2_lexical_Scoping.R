## This assignment is about solving the inverse of a matrix by caching the 
# result within a lexical scope of a function:  "makeCacheMatrix" and 
# "cacheSolve". Caching is about using memory to avoid excess computation.
# Lexical scopes, allow to create functions within a function and new 
# "user defined" objects (data types) to store data within several environments
###################################

###################################
## The function "makeCacheMatrix" creates a new, unique environment. 
# The inverse matrix is cached inside the object m, within the main 
# environment, which is unique for EACH instance the function is called.
## The output of the function is a list with 5 named elements, which are 
# the five functions defined herein: setmatrix, getmatrix, setinverse, 
# getinverse and getenv
###################################

makeCacheMatrix <- function(x = matrix()) {
  # Example input: Insert matrix e.g x<-matrix(rnorm(64),8,8)
  ## To check cached values: 
  # xMat<-makeCacheMatrix(x)  # Run the function
  # parent.env(xMat$getenv())$m  # Check the cached mean
  # environment(xMat$getmean)  # refer to environment of "m"
  m<-NULL  # assigns NULL to a variable within the current environment 
  evn <- environment()  # Save environment
  y<-NULL 
  
  setmatrix<-function(y){  # Set matrix value
    x<<-y  # cache the matrix - assigns value y from parent environment
    m<<-NULL # search through parent environments for an existing definition of the variable and set to NULL
  }
  
  getmatrix<-function() x  # Get the matrix value cached with setmatrix
  setinverse<-function(solve) m<<- solve  # Cached value of inverse matrix is saved in m
  getinverse<-function() m  # Get the saved value of inverse matrix m that was saved with setinverse
  getenv<- function() environment()
  
  list (setmatrix=setmatrix, getmatrix = getmatrix, # creates list to house the four functions  
        setinverse = setinverse,
        getinverse = getinverse,
        getenv = getenv)
  
}

###################################
## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function, e.g. xMat$getmatrix()
###################################

cacheSolve <- function(xMat= m(), ...) {
  ## Return a matrix that is the inverse of 'x'
  # Run function e.g. like this: minv<-cacheSolve(xMat = m)
  # Compares matrix to what was there before!
  m <- xMat$getinverse() # if an inverse has already been calculated this gets it
  if(!is.null(m)){ # check to see if cacheSolve has been run before
    if(xMat$setmatrix() == xMat$getmatrix()) { # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
      message("getting cached data")
      matrix<-xMat$get()
      m<-solve(matrix, ...)
      xMat$setmatrix(m)
      return(m) 
    }
    # otherwise 
    y <- xMat$getmatrix() # run the getmatrix function to get the value of the input matrix
    xMat$setmatrix(y) # run the setmatrix function on the input matrix to cache it
    m <- solve(y, ...) # compute the value of the inverse of the input matrix
    xMat$setinverse(m) # run the setinverse function on the inverse to cache the inverse
    m # return the inverse
  }
  # End
}