## The following two functions cache an inverse of a matrix. The final command to do this should look like: 
## > x<- makeCacheMatrix(matrix(c(),m,m))
## > cacheInverse(x)

## The first function makeCacheMatrix stores four functions (in form of a list) that can be used for 
## a) resetting values of a matrix stored in makeCacheMatrix function, 
## b) returning the metrix stored in makeCacheMatrix function,
## c) resetting values of an inverse matrix stored in makeCacheMatrix function,
## d) returning the inverse matrix stored in makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(solve) inverseMatrix <<- solve
  getInverse <- function() inverseMatrix
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The second function cacheSolve returns cached inverse matrix if one was already computed.
## If not, it will compute the inverse matrix from the matrix given by the user. 
## When returning cached inverse matrix, the function will notify the user about this fact. 

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("The following output shows cached data.")
    return(inverseMatrix)
  }
  data <- x$getMatrix()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
