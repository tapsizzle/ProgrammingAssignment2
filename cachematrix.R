## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        
        invMat <- NULL
        matrixAtTimeOfInverse <- NULL
        
        ##this function sets the value of the matrix to the argument y, passed in
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        
        ##this function simply returns the value of the matrix x that is currently in scope
        get <- function() x
        
        ##this function solves the inverse of the matrix to the value passed in, and also 
        ##caches the value of the matrix, at the time of setting the inverse. This way, when the 
        ##cacheSolve is done later, "matrixAtTimeOfInverse" can be checked to see if the value of the
        ##matrix has changed.
        setInv <- function(data, ...) 
        {
                invMat <<- solve(data, ...)
                matrixAtTimeOfInverse <<- data
        }
        getInv <- function() invMat
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## cacheSolve computes the inverse of the special matrix from makeCacheMatrix.  
##If the inverse was already calculated and put into cache, then this function will retrieve the inverse from the existing cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInv()
        data <- x$get()
        
        ## 
        if(!is.null(invMat) & data == matrixAtTimeOfInverse) {
                message("getting cached data")
                return(invMat)
        }
        
        x$setInv(data, ...)
        invMat
        
}
