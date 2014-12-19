##The first function below creates a special "matrix" object that caches its inverse.  Testing has shown that
##the inverted matrix computed by this function and stored is identical to the inverted matrix computed by the
##solve() command used directly on the input matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL      ## Initializes matrix container 'inverse' to empty (NULL) each pass through the
                             ## function
        
        set <- function(y) { ## first of the 4 stored elements created by this function
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x  ## second of the 4 stored elements created by this function
        
        setinverse <- function(solve) inverse <<- solve ## third stored element
        
        getinverse <- function() inverse  ## last element stored by makeCacheMatrix(), either a null matrix 
                                          ## or an inverted matrix.
        list(set = set, get = get,  ##list created from the above four elements to 'survive' this function.
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function below retrieves from cache or computes the inverse of a matrix entered into makeCacheMatrix()
## above utilizing the list created by makeCacheMatrix() and the information stored as a result. If the inverse
## has already been calculated (and the matrix has not changed), then cacheSolve() retrieves 'inverse' from
## the cache, supposedly saving time.  Timing tests have shown that running the function twice on the same
## matrix (1000X1000) reduces the amount of time required for processing once the inverse is cached. 


## Initial timings: user  system elapsed   ##Second time through: user  system elapsed
##                 0.143   0.001   0.068   ##                        0       0       0



cacheSolve <- function(x, ...) {  
        
        inverse <- x$getinverse()  ## inverted matrix is loaded from above
        if(!is.null(inverse)) {  ## if that matrix is empty, the flow bypasses the next two lines
                message("Data already cached!")  ## this line is printed if the 'inverse' was previously computed.
                return(inverse)                 ## and the already computed 'inverse' is used, thus saving time.      
        }
        
        data <- x$get()              ## this part of the function computes 'inverse' if there was no cached matrix
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        
        inverse                      ## and this is the returned, newly calculated, inverted matrix.
}
