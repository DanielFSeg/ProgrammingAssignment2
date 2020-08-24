## Pair of functions that cache the inverse of a matrix
## Pass the result of a makeCacheMatrix call to cacheSolve 

makeCacheMatrix <- function(x = matrix()) { # Allows to create a new object that
                                            # stores a matrix and its inverse
    
    inv <- NULL # Create the inverse matrix variable placeholder
    
    set <- function(y) { # Set a new matrix in the cache and clear the inverse
        x <<- y          # cache for a new matrix
        inv <<- NULL     #
    }
    
    get <- function() x  # Get the original matrix
    
    setinverse <- function(inverse) inv <<- inverse # Set the invermatrix, not
    # the calculations
    
    getinverse <- function() inv   # Get the stored inverse matrix
    
    list(set = set, get = get,     # Pool list for the functions created
         setinverse = setinverse,  # which allows the use of $
         getinverse = getinverse)
}


#' Compute and cache the inverse of a matrix
#' using the function created above
#' 

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse() # Store the inverse matrix in the cache
    
    if(!is.null(inv)) {                           # Check if the inverse matrix exists
        message("getting cached matrix inverse")
        return(inv)
    }
    
    data <- x$get() # Get the original matrix
    
    inv <- solve(data, ...) # Calculate the inverse matrix
    
    x$setinverse(inv) # Set the inverse matrix in the cache
    
    inv # return the inverse matrix
}

# Examples ----

dfs <- matrix(rnorm(25), 5, 5) # Creating a random 5x5 matrix

dfs_cache <- makeCacheMatrix(dfs) # Store the matrix with the function
                                  # makeCacheMatrix() in the new object

dfs_cache$get() == dfs # Checking if cache and matrix are the same

cacheSolve(dfs_cache) # Calculate and store the inverse matrix in the object

dfs_cache$getinverse() == solve(dfs) # Checking if cache and inverse matrix are the same

