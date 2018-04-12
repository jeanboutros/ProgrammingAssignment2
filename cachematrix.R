## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  cacheInvMatrix <- NULL
    
  set <- function(newMatrix) {
    x <<- newMatrix
    cacheInvMatrix <<- NULL  
  }
  
  get <- function() cacheMatrix
  
  getInvMatrix <- function() cacheInvMatrix
  
  setInvMatrix <- function(invMatrix) cacheInvMatrix <<- invMatrix
  
  list(set = set, get = get, getInvMatrix = getInvMatrix, setInvMatrix = setInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInvMatrix()
  if(!is.null(invMatrix)) {
    print("Cached inverse matrix")
    return(invMatrix)
  }
  
  cachedMatrix <- x$get()
  invMatrix <- solve(cachedMatrix)
  x$setInvMatrix(invMatrix)
  invMatrix
}
mm <- matrix(rnorm(9,0,1), nrow = 3, ncol = 3)
mm
mmm <- makeCacheMatrix(mm)

