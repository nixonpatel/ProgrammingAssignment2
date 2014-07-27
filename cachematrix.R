## @@makeCacheMatrix functon is a function that creates a special "matrix" object that can cache
## its inverse. It is assumed that the matrix supplied is always invertible
## @@cacheSolve function is a funtion that creates the inverse of the special "matrix" object 
## returned by makeCacheMatric function above; It returns the cached version of the previously
## calculated inverse if the matrix has not changed else computes the inverse and returns the 
## freshly computed inverse object for the matrix

## Creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix i
  i <- NULL
  
  ## set method to set the matrix
  
  set <- function(matrix) {
          m <<- matrix
          i << - NULL
  }
  
  ## get method to get the matrix

  get <- function() {
          m
  }
  
  ## setInv Method to set the inverse of the matrix
  
  setInv <- function(inv) {
            i <<- inv
  }
  
  ## getInv Method to get the inverse of the matrix
  
  getInv <- function() {
    ## Return the inverse matrix
            i
  }
  
  ## Return a list of the methods
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


  ## Computes the inverse. If the inverse has already been calculated  changed),
  ## (and the matrix has not changed) and the matrix is a square matrix then 
  ## the cachesolve should retrieve the inverse from the cache, else it can be
  ## calculated by using the svd (Singular Value Decomposition) function

  cacheSolve <- function(x, ...) {
        i  <- x$getInv()
        if (!is.null(i)){
          message("getting cached data")
          return(i)
  }
        data  <- x$get()
        i  <- solve(data, ...)
        x$setInv(i)
        i
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
