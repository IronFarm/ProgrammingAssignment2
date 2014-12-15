## Function to create a matrix whose inverse can be cached, as well as the
## function needed to retrieve the cached value

## Create a matrix object which supports caching of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    # Define an empty variable to hold the inverse
    i <- NULL
    
    # Function to set the contents of the matrix, clearing the cached value of
    # the inverse in the process
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # Function to retrieve the matrix
    get <- function() x
    
    # Functions to get and set the inverse matrix
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    
    # Return a list containing the four getter and setter functions
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## Return the inverse of the matrix, using the cached value if possible
cacheSolve <- function(x, ...) {
    # Retrieve the cached value
    i <- x$getinv()
    
    # Return the cached value if it is valid
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # Otherwise, calculate the inverse using solve() then cache and return the
    # result
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
