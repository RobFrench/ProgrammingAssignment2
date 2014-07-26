## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        ##set the inverse of the input matrix
        setinverse <- function(solve) m <<- solve
        
        ##retreive inverse matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve creates the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
        ##check for a cached value for the inverse and return if present
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##if no cache exists create the invesrse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}