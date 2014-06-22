## This pair of functions enable the results
## of the costly inverse matrix function to be 
## stored in cache(the calling environment)
## this helps the user return the results faster
## if the results had already been calculated

## this function creates a list of 4 functions
## that are used to store the results of the
## cacheSolve function in the calling enviornment(cache)
makeCacheMatrix <- function(x = matrix()) {
  ## initializes the argument x as the empty matrix
  ## that will take the parameters past in the
  ## cachesolve function
  
  s <- NULL
  ## initializes s to it's default value of NULL
  ## s has a default value of Null so that the 
  ## getSolve function will see a null when
  ## it looks for s in the defining enviornment
  
  set <- function(y) {
    ## creates a function that takes the parameter y
    ## and sets the dataset (the matrix to be inversed)
    ## to y where why is then set in the calling
    ## envionrment (the inside of cachemean)
    
    x <<- y
      ## this syntax will replace x with the
      ## paremter y which is populated by
      ## calling the $set function
      ## and replace x(the defining dataset) 
      ## with y(the calling dataset)
    
    s <<- NULL
      ## this replaces s with Null in the 
      ## calling environment so it will
      ## start fresh with a new solve upon
      ## the user passing new parameters
      ## to the set function
  }
  
  get <- function() x
    ## creates a function that retrieves x from the 
    ## defining environment (the global environment)
  
  setsolve <- function(solve) s <<- solve
    ## creates a function that takes the parameter 
    ## solve and sets s(which is the result of solve)
    ## to the results of solve from the calling
    ## environment and pushes it to the defining
    ## environment
  
  getsolve <- function() s
    ## creates a function that returns the answer
    ## s from the defining environment
  
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)
    ## outputs a list of four functions to be used
    ## within the cacheSolve function (so the 
    ## results are returned from within the calling
    ## environment)
  
}


## this function and either calculates 
## the inverse of the matrix or if it exists
## in the cache it returns the result
## of the previous calculation from cache
cacheSolve <- function(x, ...) {
  ## this function takes the parameter x 
  ## (which is the matrix to be inverted)
  
  s <- x$getsolve()
  ## s is being set to the results of passing
  ## x through the getsolve function 
  
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
    ## if s is not null in the defining environment 
    ## (this would be if setsolve had already been called)
    ## returns the message "getting cached data"
    ## then returns the result of solve
  
  data <- x$get()
    ## this is the else, if the solve was Null data x
    ## is returned from the defining environment
  
  s <- solve(data, ...)
    ## continueing with the else, if s was Null data
    ## x from the defining environment is calculated
  
  x$setsolve(s)
    ## takes the results of inversing the matrix s
    ## and pushes it to the defining environment
    ## so the next time the function is called
    ## the results will be NULL
  
  s
    ## returns s which is the inverse of the matrix
    ## that was either just calculated or retunred
    ## from cache
      
}
