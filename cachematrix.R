## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y =matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inversematrix) m <<- inversematrix
  getinverse <- function() m
  
  matrix(list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse))

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 ## Gets Inverse function is called 
 m <- x[[4]]()

  if(!is.null(m)) {
    message("getting cached data")
    
    return(m)
  }
 ## Gets  function is called 
  data <- x[[2]]()
  m <- solve(data)
  
  ## Set Inverse function is called 
  x[[3]](m)
  m
}

