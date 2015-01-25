## This function creates a special "matrix" object that can
## cache its inverse. 

## makeCacheMatrix function contains 4 member functions: set,
## get, setInv, getInv. This function uses <<- assignment 
## operator so that these internal variables can be assigned
## a value that is different from the outside environment.


makeCacheMatrix <- function(x = matrix()) {
  
    xinv <- NULL
    set <- function(y) {
            x <<- y
            xinv <<- NULL
    } 
    get <- function() x
    setInv <- function(inv) xinv <<- inv
    getInv <- function() xinv
    list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the
## cache. 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
  
}

