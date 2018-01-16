## The functions below cache the invese of a matrix

## The function below creates a special matrix object that can cache its inverse

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


## The function below computes the inverse of the matrix returned by makeCacheMatrix

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
