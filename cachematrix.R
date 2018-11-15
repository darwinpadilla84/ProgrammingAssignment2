## Put comments here that give an overall description of what your
## functions do

# This function helps to create the inverse matrix 
#in a list that establishes the inverse matrix, 
#obtains the values of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL 
    }
    get <- function()x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}

#This function calculates the inverse matrix created 
#in the previous function, but first verifies if the 
#inverse matrix has been calculated. If not, skip the 
#calculation.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
