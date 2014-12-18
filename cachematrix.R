## The two functions below will cache a matrix and find its inverse
## but will improve run time by providing the cached inverse if the
## matrix is the same.

## This function will store the matrix 'x' and the inverse 'm'. 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function will either compute and return the inverse of the
## matrix 'x' or will provide the cached data 'm' if it is not null.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
