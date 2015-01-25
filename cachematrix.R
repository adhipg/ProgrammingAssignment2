## Cache Matrix Inverses

## Functions that allow you to compute the inverse of a matrix
## and cache the results allowing subsequent computations to be faster.

## Creates the special matrix object that can also store cached results.
## Allows a matrix to have additional four functions:
## - get
## - set
## - setinverse
## - getinverse
##
## Usage: x$getinverse() etc.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list( set = set, get = get, setmean = setmean, getmean = getmean )
}


# Compute the inverse of a matrix.
# Returns back the cached inverse of the matrix if the matrix has already been
# calculated (and it hasn't changed)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ## check if we have cached data for this matrix
        if( !is.null(inv) ) {
                message("getting cached data")
                return(inv)
        }

        ## we don't have cached data. Calcuate inverse, cache it and return it.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
