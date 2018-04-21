## cacheSolve function takes as parameter a special list object 
## and returns the inverse of the initial matrix that was previously
## formatted by makeCacheMatrix

## makeCacheMatrix function create a special list from a matrix
##
## 'inMatrix' is a matrix (square or not)
##
## Returns a list containing the following functionnal attributes:
## set,get,setinverse and getinverse

makeCacheMatrix <- function(inMatrix=matrix()) {
    matinv <- NULL
    set <- function(setMatrix) {
      inMatrix <<- setMatrix
      matinv <<- NULL
    }
    get <- function() inMatrix
    setinverse <- function(inversed) matinv <<- inversed
    getinverse <- function() matinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function compute the inverse of a special matrix
## returned by the function makeCacheMatrix
##
## 'inObject' is list returned by the function makeCacheMatrix
##
## Returns a square matrix that is the inverse of the initial one
## that was given to makeCacheMatrix.

cacheSolve <- function(inObject, ...) {
  #Get the dimension of matrix
  nbRows = dim(inObject$get())[1]
  nbCols = dim(inObject$get())[2]
  # check if not a square matrix
  if (nbRows != nbCols) {
    message("Provided object does not contain a square matrix.")
  }
  else { # square matrix provided
        matinv <- inObject$getinverse()
        if(!is.null(matinv) && identical(inObject$get(), solve(matinv))) {
          message("Returning cached inverse.")
          return(matinv)
        }
        data <- inObject$get()
        matinv <- solve(data)
        inObject$setinverse(matinv)
        return(matinv)
  }
}