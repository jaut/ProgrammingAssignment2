## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
## It's really a list of functions.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   # Set inverse to null
    set <- function(y) {  # Set the matrix to y, and clear the cached inverse
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve # This function sets the inverse of the matrix
    getinv <- function() inv   # This returns the inverse 
    list(set = set, get = get, # This returns the functions as a list for use.
         setinv = setinv,
         getinv = getinv)    
}

## This function generates the matrix inverse and stores it in the cache, if it
## doesn't already exist. If it does exist, it retrieves the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()  # get the inverse matrix value from the function
    if(!is.null(inv)) {  # if the cached value is not null, it's been computed, 
        message("getting cached data") # so return that matrix and let us know
        return(inv)  # we're getting cached data.
    }
    data <- x$get() # Otherwise, we need to compute it. Get the matrix,
    inv <- solve(data, ...)  # compute the inverse matrix, and put the computed
    x$setinv(inv)  # matrix into the cache for later use,
    inv  # and return the inverted matrix we just computed and stored.
}
