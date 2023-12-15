## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {


 ## Initialize the inverse property
    inv <- NULL

    ## Method to set the matrix
    setMatrix <- function(matrix) {
        x <<- matrix
        inv <<- NULL
    }

    ## Method to get the matrix
    getMatrix <- function() {
        ## Return the matrix
        x
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        inv
    }

    ## Return a list of the methods
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()

    ## Just return the inverse if it's already set
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## Get the matrix from our object
    data <- x$getMatrix()

    ## Calculate the inverse using matrix multiplication
    inv <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(inv)

    ## Return the matrix
    inv
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
