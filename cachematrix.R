## These cache functions invert a matrix with less time.

## This assigns a list of items to be used in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        inverted.matrix <- NULL
        set <- function(y) {
                x <<- y
                inverted.matrix <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverted.matrix <<- solve
        getsolve <- function() inverted.matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This draws objects from makeCacheMatrix to calculate an inverted matrix more easily.

cacheSolve <- function(x, ...) {
        inverted.matrix <- x$getinvert()
        if(!is.null(inverted.matrix)) {
                message("getting cached matrix")
                return(inverted.matrix)
        }
        data <- x$get()
        inverted.matrix <- solve(data, ...)
        x$setsolve(inverted.matrix)
        inverted.matrix
}