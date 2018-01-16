## The functions below cache the invese of a matrix

## The function below creates a special matrix object that stores (cache) its inverse.
## It also allow to set and get the matrix and its inverse matrix. Note: it uses special assignment operator (<<-) to store the inverse.
makeCacheMatrix <- function(x = matrix()) {
        storedInverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                storedInverse <<- NULL
        }
        getMatrix <- function() x
        setinverse <- function(inverse) storedInverse <<- inverse
        getinverse <- function() storedInverse
        list(setMatrix = setMatrix, getMatrix = getMatrix, setinverse = setinverse, getinverse = getinverse)
}


## The function below computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse is already calculated and the matrix is not changed, this function returns or prints the inverse from the stored (cached)
## inverse 
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
