## makeCacheMatrix creates a list containing a function that sets the value of the matrix, 
## gets the value of the matrix, sets the value of inverse of the maqtrix and gets the value
## of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list( set = set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse )
}

## This function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse and sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
        i = x$getinverse()
        if (!is.null(i)) {
                message("reading from cache")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        message("writing to cache")
        x$setinverse(i)
        i
} 
