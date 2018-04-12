## This is a sample project of how to create a matrix object that can store the inverse of the 
## matrix as well and how to create a function that handles the creation and the caching
## of the inverse of a matrix.

## This object takes a matrix as an argument and has a method to store the inverse
## of the matrix that was provided when the object was constructed

makeCacheMatrix <- function(x = matrix()) {
  
  #This is the normal matrix that will be received when constructing the object
  cacheInvMatrix <- NULL
    
  #The object's main setter
  set <- function(newMatrix) {
    x <<- newMatrix
    
    #Leave the Inverse of the matrix null for now
    #A caching function will take care of setting this variable
    cacheInvMatrix <<- NULL  
  }
  
  #The object's getter that returns the received matrix
  get <- function() cacheMatrix
  
  #A getter for the inverse of the matrix
  getInvMatrix <- function() cacheInvMatrix
  
  #A setter to assign a value for the inverse of the matrix
  setInvMatrix <- function(invMatrix) cacheInvMatrix <<- invMatrix
  
  #Create the object...
  list(set = set, get = get, getInvMatrix = getInvMatrix, setInvMatrix = setInvMatrix)
}


## This function handles the caching of the inverse of a matrix.
## If the inverse doesn't exist, this function creates it using the 
## solve() function and sets it using the object's setter

cacheSolve <- function(x, ...) {
  # Get the value of the inverse of the matrix
  invMatrix <- x$getInvMatrix()
  # if the value is not null then return it! (It means it's cached)
  if(!is.null(invMatrix)) {
    print("Cached inverse matrix")
    return(invMatrix)
  }
  
  # If the code reaches this line then it hasn't entered the above If-block
  # and this means that the value of the inverse of the matrix has not been calculated yet.
  cachedMatrix <- x$get()
  # Compute the inverse of the matrix
  invMatrix <- solve(cachedMatrix)
  # Set the value of the object's "inverse of the matrix"
  x$setInvMatrix(invMatrix)
  
  ## Return a matrix that is the inverse of 'x'
  invMatrix
}

