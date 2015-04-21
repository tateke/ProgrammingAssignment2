## The following functions help to allieviate the costly computation required to inverse a matrix.
## If the matrix has already been cached then it will retrieve the cache instead instead of doing
## the inversion again


makeCacheMatrix <- function(x = matrix()) {
    #Set m (local variable) to NULL (undefined object) 
    m <- NULL
    
    #Create a nested function set
    set <- function(y) {
        #Check to see if the variables were already assigned in the parent environment
        #If no then assign them here
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setSolve <- function(Solve) m <<- Solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


cacheSolve <- function(x, ...) {
    # Declare a local variable inside this funciton and set it to the passed in
    # matrix subsetted by the getSolve function
    m <- x$getSolve()
    # If m is not null then we already have the data cached and we can simply
    # return m which is the cached inversed matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # The data is not cached so now we need to inverse the passed in matrix by
    # using solve
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}