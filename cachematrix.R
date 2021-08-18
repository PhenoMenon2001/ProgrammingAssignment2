## Sets value for matrix that is to be used later to compute inverse. Also sets variable that will receive inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set<- function(y){
    x <<- y
    inverseMatrix <<- NULL
    
  }
  get <- function() x
  setInverseMatrix <- function(solvedMatrix) inverseMatrix <<- solvedMatrix
  getInverseMatrix <- function () inverseMatrix
  
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
  
}


## Checks whether matrix is not empty, computes the inverse matrix, and returns the inverse

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  matrixdata <- x$get()
  inverseMatrix <- solve(matrixdata)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
  
}
