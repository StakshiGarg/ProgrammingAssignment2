## Put comments here that give an overall description of what your
## functions do

## This function converts matrix to cached matrix with inverse. Takes matrix as input

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL ## inverse for matrix
	## sets matrix contents in case they need to change  
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## get matrix in case it is cached
        get <- function() x
        ## set inverse after calculating
        setinverse <- function(inverse) i <<- inverse
        ## get cached inverse
        getinverse <- function() i
        ## sets all functions for enabling cache matrix
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}


## this function solves(inverse) matrix and caches solution 

cacheSolve <- function(x, ...) {
	## gets inverse of matrix to check if available
        i <- x$getinverse ()
        ## if inverse is available send it from cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## get cached matrix
        data <- x$get()
        ## solves matrix
        i <- solve(data)
        ## save inverse
        x$setinverse (i)
        i
}
