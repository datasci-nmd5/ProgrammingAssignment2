##The following set of functions allow users to create and cache the inverse of  
##a matrix so that it can be retrieved for use (if the matrix is unchanaged)
##rather than repeatedly solving for the inverse (during a loop, for example)


##This function, creates a special "vector" which is really a list containing a 
##function to:
##1 - set the value of the matrix
##2 - get the value of the matrix
##3 - set the value of the inverse matrix
##4 - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        imat <- NULL       
        setmat <- function(y) {
                x <<- y
                imat <<- NULL
        }
        getmat <- function() x
        setinvmat <- function(solve) imat <<- solve(x)
        getinvmat <- function() imat
        list(setmat = setmat, getmat = getmat,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}

##The following function returns the inverse of a matrix, making use 
##of the special "vector"created above. 
##1 - First, checks to see if the inverse has already been calculated and
##      cached in the special "vector" in the previous function. If so, it gets  
##      the inverse from the cache and skips the computation. 
##2 - Otherwise, solves the inverse of the matrix and sets the value of the 
##      inverse in the cache.

cacheSolve <- function(x, ...) {
        imat <- x$getinvmat()
        if(!is.null(imat)) {
                message("getting cached data")
                return(imat)
        }
        data <- x$getmat()
        imat <- solve(data)
        x$setinvmat(imat)
        imat
}