## 
## Functions to cache the inverse of a matrix 
## (Assumption:  matrix supplied is always invertible)


## This function creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            
            ## set function - sets x to the inputted value, sets m to null 
            set <- function (y) {
                    x <<- y
                    m <<- NULL
            }
            
            ## get function - returns x (which is what was set in the set function)
            get <- function () x
            
            ## setinverse function - takes the inverse input parameter and assigns to m
            setinverse <- function(inverse) m <<- inverse
            
            ## getinverse function - just returns m (which is the inverse set in setinverse)
            getinverse <- function() m
            
            ## creates the list with all the functions in it (and returns it)       
            list(set = set,
                 get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
            
}




##This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        
        ## goes and gets the cached inverse (may or not be set)
        m <- x$getinverse()
        
        ## if m is already set - output message and return the data
        if (!is.null(m)) {
                message("getting cached data and returning it")
                return(m)
        }
        
        ## otherwise - continue processing
        ## go and get the data
        data <- x$get()
        
        # calculate the inverse of it
        m <- solve(data)
        
        # set the inverse (for another time)
        x$setinverse(m)
        
        # then return the data
        m

}
