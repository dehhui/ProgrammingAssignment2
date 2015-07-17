
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## sets the matrix 
  m <- NULL
  set <- function(y =matrix()) {
    x <<- y
    m <<- NULL
  }
  
  ## gets the matrix
  get <- function() x
  
  ## sets the inverse 
  setinverse <- function(inversematrix) m <<- inversematrix
  
  ## gets the inverse 
  getinverse <- function() m
  
  ## returns the matrix object 
  matrix(list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse))

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
 
 ## getinverse() function is called 
 m <- x[[4]]()

  if(!is.null(m)) {
    message("getting cached data")
    
    return(m)
  }
 ## get() function is called 
  data <- x[[2]]()
  m <- solve(data)
  
  ## setinverse function is called 
  x[[3]](m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}

