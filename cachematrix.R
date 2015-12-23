## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix
## inversion that we will not discuss here). 
## This program writes a pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invmat <<- inverse
        getinverse <- function() invmat
        list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        invmat <- x$getinverse()
        if (!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinverse(invmat)
        invmat       
}
## Solutions
## > mera<-makeCacheMatrix(matrix(5:8,2,2))
## > cacheSolve(mera)
##      [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > cacheSolve(mera)
## getting cached data
##      [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > mera$getinverse()
##      [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > 