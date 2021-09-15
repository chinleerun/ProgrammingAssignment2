## This file include two function: makeCacheMatrix and cacheSolve.
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix

makeCacheMatrix <- function(X = matrix()) {
        s <- NULL
        set <- function(y) {
                X <<- y
                s <<- NULL
        }
        get <- function() X
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve

cacheSolve <- function(XX, ...) {
        s <- XX$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- XX$get()
        s <- solve(data,...)
        XX$setinverse(s)
        s
}
