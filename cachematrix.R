## Given a invertible matrix, the following two functions will calculate the inverse matrix or 
## retrieve the inverse matrix from the cache.
## Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse. 
## makeCacheMatrix contains 4 functions: set, get, setmean, getmean.

## get is a function that returns the vector x stored in the main function.
## set is a function that changes the vector stored in the main function.
## setinverse and getinverse are functions are very similar to set and get.
## They simply store the value of the input in a variable m
## into the main function makeVector (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y){
                x <<- y
                m <<- NULL
        }
    
        get <- function()x
    
        setinverse <- function(solve) {m <<- solve()}
        
        getinverse <- function() {m}
    
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function “cacheSolve” computes the inverse of the special “matrix” 
## (which is the input of cachemean) returned by makeCacheMatrix above
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        m <- x$getinverse()
        
        if(!is.null(m)){
                message("Getting Cached Data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
