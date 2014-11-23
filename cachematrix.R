## The following functions makeCacheMatrix() and cacheSolve() are used 
## to implement calculating inverse of a matrix, but take advantage of the cache
## if it has already been calculated

## makeCacheMatrix Function 
## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##   this list is used as the input to cacheSolve()


makeCacheMatrix <- function(x = matrix()) {

   
  invMat = NULL
  
  
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    invMat <<- NULL
  }
  
  get = function() x
  setinv = function(inverse) invMat <<- inverse 
  getinv = function() invMat
  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)
  
  
}


## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat = x$getinv()
  
  # Asscertain if the inverse has been calculated earier 
  
  if (!is.null(invMat)){
    message("Cache Check :  Using Cached Data")
    return(invMat)
  }
  
  mat.data = x$get()
  
  # solve function call - first argument is the matrix ; second arg is missing to get an inverse
  
  invMat = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  
  x$setinv(invMat)
  
  return(invMat)
  
}
