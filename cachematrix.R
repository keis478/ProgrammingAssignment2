## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse to NULL
        
        # Function to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset the inverse cache when the matrix changes
        }
        
        # Function to get the matrix
        get <- function() x
        
        # Function to set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
                
        # Function to get the inverse of the matrix
        getInverse <- function() inv
        
        # Return a list of the functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()  # Get the cached inverse
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)  # Return the cached inverse if it exists
        }
        
        data <- x$get()  # Get the matrix
        inv <- solve(data, ...)  # Compute the inverse
        x$setInverse(inv)  # Cache the inverse
        inv  # Return the inverse
}
