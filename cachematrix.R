# cachematrix.R -- This file contains two functions:
#  1. makeCacheMatrix() this funtion caches a square matrix and its inverse. It
#     returns the cached inverse if the matrix hasn't changed.
#  2. cacheSolve() gets the matrix inverse. It will attempt to return the cached
#     inverse in the event the matrix has not changed.


# makeCacheMatrix() -- provides an interface for getting/setting a matrix and
#                      getting/setting the matrix inverse so that the inverse
#                      may be cached.

makeCacheMatrix <- function(x = matrix()) {
    
    # Initially the inverse is NULL
    x_inv <- NULL
    
    # Set the matrix to a new matrix.
    set <- function(y)
    {
        x <<- y
        x_inv <<- NULL
    }
    
    # Return the matrix.
    get <- function() x
    
    # Store the cached inverse.
    set_inverse <- function(inv)  x_inv <<- inv
    
    # Return the cached inverse.
    get_inverse <- function() x_inv
    
    # Setup the list of functions defined in makeCacheMatrix()
    list( set = set,
          get = get,
          set_inverse = set_inverse,
          get_inverse = get_inverse)

}

# cacheSolve() -- returns the inverse of a matrix x. If x has not changed, the
#                 previously calculated inverse can be returned to avoid recomputation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # Get the cached inverse.
    x_inv <- x$get_inverse()
    
    # If the inverse has been cached, return this value.
    if ( !is.null(x_inv))
    {
        message("Retrieving the cached inverse.")
        return(x_inv)
    }
    
    # Else, get the matrix,
    data <- x$get()
    
    #       compute its inverse,
    x_inv <- solve(data, ...)
    
    #       cache the inverse,
    x$set_inverse(x_inv)
    
    #       and return the inverse.
    x_inv
}
