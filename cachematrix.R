## makeCacheMatrix and cachSolve work together to cache the inverse
## of matrices so that this costly computation needs not be repeated
## for a given matrix

##====================================================================
## Example code: 
## x <- matrix(rnorm(9), nrow = 3, ncol = 3)
## solve(x)
## test_inv <- makeCacheMatrix(x)
## cacheSolve(test_inv)
## cacheSolve(test_inv)
##====================================================================

## makeCacheMatrix takes a matrix object and returns a list that allows:
## 1) setting the value of the matrix
## 2) getting the value of the matrix
## 3) setting the inverse of the matrix
## 4) getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(inverse) {inv <<- inverse}
        getinv <- function() {inv}
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##====================================================================

## cacheSolve takes the object created by makeCacheMatrix 
## 1) returns the cached inverse matrix if it is already calculated
## otherwise 
## 2) calculate the inverse and cache it via setinv() in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
