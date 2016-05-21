#  Matrix inversion is usually a costly computation and so we want 
#  to cache the inverse of a matrix rather than computing it repeatedly.
#  This is a pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # set invm (inverse matrix) to null
  invm <- NULL 

    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invm <<- inverse
    getinverse <- function() invm
    list(set=set, 
    	 get=get, 
    	 setinverse=setinverse, 
    	 getinverse=getinverse
    	 )
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
         invm <- x$getinverse()
    # check if value of inverse matrix has been calculated.  If TRUE, return cache value     
    if(!is.null(invm)) {
        message("getting cached data.")
        return(invm)
    }
    # Return a matrix that is the inverse of 'x' using the solve() function
    orig <- x$get()
    invm <- solve(orig)
    x$setinverse(invm)
    invm
}
