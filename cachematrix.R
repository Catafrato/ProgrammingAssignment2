## This file contains two functions that will use R's lexical scoping feature
## to cache the inverses of matrices,
## thus saving up on computation time should
## the inverses have to be computed again.

## This function is used to initialize a matrix that will
## be cached along with its inverse. It has four sub-functions.
## It is meant to be used directly by the user
## ONLY to initialize the INITIAL matrices. All the necessary interactions
## with it will be done by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        
        getinverse <- function() m
        
        list(set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of a matrix initialized by
## the makeCacheMatrix function. If the inverse of a given matrix is already
## cached, it will simply return the cached version, saving up on time.
## Otherwise, it will perform the computation and cache the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
