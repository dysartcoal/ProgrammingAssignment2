## The functions in this file create an object which can cache a matrix with
## its inverse. The inverse is calculated only once per matrix assignment.

## makeCacheMatrix()
## Arg: x: invertible square matrix.  Default:  empty matrix.
## Returns a list of functions for the matrix, x, and its inverse. The
## matrix x provided as an argument will be assigned in the environment
## but the inverse is not calculated and remains null.
## 
## The functions returned in the list are:
##  - set: sets the matrix cached with the object
##  - get: gets the matrix 
##  - setinverse: sets the value of the inverse for the matrix
##  - getinverse: gets the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(invmatrix) inv <<- invmatrix
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve()
## Arg: x: An object returned from the makeCacheMatrix() function.
## Returns the inverse of the matrix associated with the object x.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## No checks: assume that x was created by makeCacheMatrix()
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    
    ## Use solve to obtain matrix inverse.
    ## No checks: assume that inv is always invertible.
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
