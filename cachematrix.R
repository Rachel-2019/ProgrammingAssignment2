## These functions serve to create a matrix, cache it,
## create the inverse of this matrix, and return this inverted matrix.

## This makeCacheMatrix function creates an object in R 
## that stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        set <- function(whatever){
                x <<- whatever
                inverted <<- NULL
        }
        get <- function() x
        setinverted <- function(inversion) inverted <<- inversion
        getinverted <- function() inverted
        list (set = set, 
             get = get, 
             setinverted = setinverted, 
             getinverted = getinverted)
        }


## This cacheSolve function uses an argument from makeCacheMatrix() 
## to retrieve the inverse from the cached value that is stored in the 
## makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        inverted <- x$getinverted()
        if(!is.null(inverted)){
                message("getting cached data")
                return(inverted)
        }
        data <- x$get()
        inverted <- solve(data, ...)
        x$setinverted(inverted)
        inverted
}
