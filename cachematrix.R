makeCacheMatrix <- function(x = matrix()) {
        # Create an object that contains a matrix and its inverse
        # Args:
        #       x: the matrix
        # Returns
        #       the object with getter and setters for the matrix and its
        #       inverse
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inv) inv <<- inv
        getInv <- function() inv
        list(set = set, get = get,
             getInv = getInv,
             setInv = setInv)
}


cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        # Only compute the inverse if it wasn't computed previously
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}

#Not run:
#x<-matrix(runif(9),nrow=3,ncol=3)
#cx<-makeCacheMatrix(x)
#print(cacheSolve(cx))
#print(cacheSolve(cx))
