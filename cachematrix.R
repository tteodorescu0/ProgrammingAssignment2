## Matrix inversion is usually a costly computation and there 
## are benefits to cache the inverse of a matrix 
## rather than computing it repeatedly. The two functions below
## cache the inverse of a matrix.

## The function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {

        # initialize the stored value of the inverse to null
        inverse <- NULL

        # set the value of the matrix
        set <- function(y) {
                x <<- y
                # re-initialize the stored value of inverse
                # to null because the function changed
                inverse <<- NULL
        }
        
        #get the value of the matrix
        get <- function() { x }
        
        # set the inverse of the matrix
        setinverse <- function(i) { inverse <<- i }
        
        # get the inverse of the matrix
        getinverse <- function() { inverse }
        
        ## returns a list of 4 functions above
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {

        # get the cached inverse; could be null
        inverse <- x$getinverse()
        
        # if inverse is cached, get the cached value, 
        # and return
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # if inverse is not cached, get value of matrix 
        data <- x$get()
        
        # then compute its inverse
        inverse <- solve(data, ...)
        
        # then set the cached value of the inverse
        x$setinverse(inverse)
        
        # and then return the inverse matrix
        inverse
  
  
}
