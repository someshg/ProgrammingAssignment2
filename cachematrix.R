## The set of functions will cache the inverse of a matrix so that repeated
## inverse operations on the same matrix will return a cached result.
## Usage:
## makeCacheMatrix(x): returns the matrix with inverse caching property
## x$set(y): where y is a matrix initializes x with the values in y
## cacheSolve(x): returns the inverse of x. First call will calculate
##                the inverse and cache it - subsequent calls will return the
##                cached value

## makeCacheMatrix(x): takes x and initializes it as a special type of matrix
##                     that supports 4 functions - set(to initialize the matrix),
##                     get(to get the matrix), setinverse(to cache the inverse) and
##                     getmatrix(to return the cached value)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(x): returns the inverse of x. First call will calculate
##                the inverse and cache it - subsequent calls will return the
##                cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

