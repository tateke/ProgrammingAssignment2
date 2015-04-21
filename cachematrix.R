## The following functions help to allieviate the costly computation required to inverse a matrix.
## If the matrix has already been cached then it will retrieve the cache instead instead of doing
## the inversion again


makeCacheMatrix <- function(x = matrix()) {

    #This stores the cached inverse matrix if it exists
    #The first time there is no cached version so the value
    # will be NULL
    cache <- NULL
    
    #Create a nested function set to store the matrix
    setMatrix <- function(MatrixValue) {
        #Assign x to the matrix value that was passed in to the function
        x <<- MatrixValue
        # Since we have a new matrix value we need to make sure that cache is NULL
        cache <<- NULL
    }
    
    # Return the matrix 
    getMatrix <- function() x
    
    # Cache the inverser of the matrix, using Solve
    SolveInverse <- function(Solve) {
        cache <<- Solve
    }
    
    # get the cached inverse matrix
    GetInverse <- function() cache
    
    # Return a list of each function
    list(setMatrix = setMatrix, getMatrix = getMatrix, SolveInverse = SolveInverse, GetInverse = GetInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(y, ...) {
    # Get the cached inverse matrix that was created in makeCacheMatrix, if it exists
    InverseMatrix <- y$GetInverse()
    
    # If there is a cached value for the matrix then return it along with a message to let the user know that the value was cached.
    if(!is.null(InverseMatrix)) {
        message("getting cached data")
        return(InverseMatrix)
    }
    
    # The data is not cached so now we need to inverse the passed in matrix by
    # using solve and then cache it
    data <- y$getMatrix()
    InverseMatrix <- solve(data, ...)
    y$SolveInverse(InverseMatrix)

    # return the newly cached inverse of the matrix
    InverseMatrix
}