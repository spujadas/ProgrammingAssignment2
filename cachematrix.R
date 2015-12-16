##############################################################################
#
# FILE
#   cachematrix.R
#
# OVERVIEW
#   Implements two functions that enable the inverse of a matrix to be
#   calculated and cached in order to speed up calculation times when the
#   inverse is needed several times.
#
# EXAMPLE
#   Initialise a matrix:
#   > m <- matrix(c(1,1,0,2), nrow = 2)
#   > m
#        [,1] [,2]
#   [1,]    1    0
#   [2,]    1    2
#
#   Create a cached-inverse matrix object from the original matrix:
#   > cm <- makeCacheMatrix(m)
#
#   Invert the matrix (the result is automatically cached in 'cm'):
#   > cacheSolve(cm)
#   [,1] [,2]
#   [1,]  1.0  0.0
#   [2,] -0.5  0.5
#
#   Request the inverted matrix again (returns the cached result):
#   > cacheSolve(cm)
#   getting cached data
#   [,1] [,2]
#   [1,]  1.0  0.0
#   [2,] -0.5  0.5
#


##############################################################################
#
# FUNCTION
#   makeCacheMatrix(x) - Creates an object that stores a matrix and can cache 
#     the inverse of this matrix. The resulting object is known as a
#     cacheMatrix.
#
# INPUT
#   x    a matrix
#
# OUTPUT
#   A cacheMatrix object that stores the input matrix (x) and that can cache
#   its inverse (the cache is initially empty, cacheSolve() populates it).
#
# SEE ALSO
#   cacheSolve()
#

makeCacheMatrix <- function(x = matrix()) {
    
    ### Initialise the attributes of cacheMatrix object
    
    ## x: the input matrix, passed to makeCacheMatrix(), is implicitly stored
    #     in the cacheMatrix object
    
    ## s: the inverted matrix (initially non-populated)
    s <- NULL
    
    
    ### Define the methods of cacheMatrix object
    
    ## set(): populates the cacheMatrix object with a new matrix to be inverted
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## get(): returns the originally input matrix
    get <- function() x
    
    ## setsolve(): inverts the input matrix and stores the result in s
    setsolve <- function(solve) s <<- solve
    
    ## getsolve(): returns the inverse of the input matrix if previously
    #     calculated and stored in s by setsolve(), otherwise (if the inverse
    #     hadn't previously been calculated) defaults to NULL
    getsolve <- function() s
    
    
    ### Return the cacheMatrix object
    
    # Note: this object explicitly includes the four methods above, which use
    #   the (implicitly included) x and s attributes
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


##############################################################################
#
# FUNCTION
#   cacheSolve(x, ...) - Returns a matrix that is the inverse of x, where x is
#     a cacheMatrix object.
#
# INPUT
#   x    a cacheMatrix, as returned by makeCacheMatrix()
#
#   ...  additional arguments, see ?solve for list of possible arguments and
#        details
#
# OUTPUT
#   A matrix that is the inverse of x. If the inverse had previously been
#   calculated, then the cached result of this calculation is returned.
#
# SEE ALSO
#   makeCacheMatrix()
#

cacheSolve <- function(x, ...) {
    
    # Attempt to retrieve the cached inverse of x
    s <- x$getsolve()
    
    # If the inverse of x was cached...
    if(!is.null(s)) {
        message("getting cached data")
        # ... then return it...
        return(s)
    }
    
    # ... otherwise...
    
    # Retrieve the matrix from the input cacheMatrix x
    data <- x$get()
    
    # Invert the matrix
    s <- solve(data, ...)
    
    # Store the result in the cacheMatrix's cache for future reuse
    x$setsolve(s)
    
    # Return the inverse of the matrix
    s
}

