## 
## Program to cache the result within a lexical scope of a function:  "makeCacheMatrix" and 
# "cacheSolve". Caching is about using memory to avoid excess computation.
# Lexical scopes, allow to create functions within a function and new 
# "user defined" objects (data types) to store data within several environments

###################################


## This function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(x = matrix((rnorm(64),8,8) {
  s <- NULL
  
  set <- function(y) { # Set matrix value
    x <<- y             # cache the matrix - assigns value y from parent environment
    s <<- NULL          # search through parent environments for an existing definition of the variable and set to NULL
  }
  
  get <- function() x  # Get the matrix value cached with setmatrix
  setsolve <- function(solve) s <<- solve  # Cached value of inverse matrix is saved in m
  
  getsolve <- function() s  # Get the saved value of inverse matrix m that was saved with setinverse
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) # creates list to house the four functions 
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  
  if(!is.null(s)) { # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
};
