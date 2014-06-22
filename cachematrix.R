## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
##
## EXAMPLE USAGE: 
## (assignment:) z <- makeCacheMatrix(matrix(nrow=4,ncol=4,runif(16,100,200)))
##
## (to show contents:) z$get()
## (don't forget the parenthesis!)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrx" returned by makeCacheMatrix.
##      if the inverse has already been calculated (and the matrix has not changed),
##      then the cacheSolve will retrieve teh inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
