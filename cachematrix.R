## The functions below cache the inverse of a matrix; if already calculated and the matrix is unchaged prints the precalcualted, stored 
## inverse.

## The function below creates a special matrix object that stores (cache) its inverse.
## It also allow to set and get the matrix and its inverse matrix. Note: it uses special assignment operator (<<-) to store the inverse.
makeCacheMatrix <- function(inputMatrix = matrix()) {
        storedInverse <- NULL
        setMatrix <- function(input) {
                inputMatrix <<- input
                storedInverse <<- NULL
        }
        getMatrix <- function() inputMatrix
        setinverse <- function(inverse) storedInverse <<- inverse
        getinverse <- function() storedInverse
        list(setMatrix = setMatrix, getMatrix = getMatrix, setinverse = setinverse, getinverse = getinverse)
}


## The function below computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse is already calculated and the matrix is not changed, this function returns or prints the inverse from the stored (cached)
## inverse 
cacheSolve <- function(inputMatrix, ...) {
        storedInverse <- inputMatrix$getinverse()
        if (!is.null(storedInverse)) {
                message("getting cached data")
                return(storedInverse)
        }
        data <- inputMatrix$getMatrix()
        calculatedInverse <- solve(data) #a base package function; produces inverse of a matrix
        inputMatrix$setinverse(calculatedInverse)
        calculatedInverse
}
