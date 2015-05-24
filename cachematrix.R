## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x # gets the matrix
        setinv <- function(inv) m <<- inv # assigns inverse
        getinv <- function() m # fetch inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## it checks if the matrix is already there by calling makeCaheMatrix functions and if it is not there then solve for it and set the new inversa through setinv
cacheSolve<- function(x, ...) {
        m <- x$getinv() # fetch the inverse from cache data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() #calls get function to assign the data 
        m <- solve(data, ...) # solve it to get inverse
        x$setinv(m) # assigns and create aditional cache data
        m
}
