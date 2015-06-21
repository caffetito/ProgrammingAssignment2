########makeCacheMatrix
########creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL    
    ########sets the matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    ########gets the value of the matrix
    get <- function() x
    ########setSolve saves the value of the solve
    setSolve <- function(solve) s <<- solve
    ########getSolve gets the value of the inverse
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

########CacheSolve gets the invese matrix from makeCacheMatrix


cacheSolve <- function(x, ...) {
    
    ########gets cached matrix saved in variable x and its inverse saved as s 
    ########if it is available   
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ########Otherwise computes the matrix from "x" and setSolve saves it to the cache
    ########and returns s
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}