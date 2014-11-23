## A pair of functions that cache the inverse of a matrix.
## Its puspose is to store a martix and a cached value of the inverse of the matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # set it to NULL initially
  m <- NULL
  
  # set a matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  # get the stored matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    m <<- inverse
  }
  
  # get the inverse of the matrix
  getInverse <- function() {
    m
  }
  
  # return a list with each element of the list a function defined above
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## return a matrix that is the inverse of x
  m <- x$getInverse()
  
  ## return the inverse if its set and cached
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  mat <- x$get()
  
  ## Calculate the inverse of the matrix
  m<-solve(mat, ...)
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## return the matrix m
  m
}







