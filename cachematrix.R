## This function creates a special "matrix" object that can cache its inverse
## Assuming that the matrix is always invertible


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(matrix1) m <<- matrix1 
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated ( and the matrix has not changed), then the cacheSolve should
## retrive the inverse from the cache

cacheSolve <- function(x=matrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) ##compute inverse of the matrix
    x$setinverse(m)
    m
}
