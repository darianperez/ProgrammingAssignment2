## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This fucntion creates a special matrix
## which consists of seting the value of the matrix,
## getting the value of the matrix, setting the
## value of the inverse, and getting the value of the
## inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { ##setting the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x  ## getting the value of the matrix
    setinverse <- function(solve) m <<- solve ##setting the value of the inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse) ## getting the value of the inverse
}


## Write a short comment describing this function
## This function calculates the inverse of the special matrix.
## If the inverse has alredy been calculated, the function gets
## the inverse from the cache and skips the computations. If the
## the matrix has not been computed, it then proceeeds with the
## computation.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()  ## setting m to the inverse
    if(!is.null(m)) {  ## if the matrix is stored in the cache
        message("getting cached data") ## prints this message
        return(m) ## returns the inverse
    }
    data <- x$get()   ## if the inverse is not stored, it computes it
    m <- solve(data, ...)
    x$setinverse(m)
    m  ## returning the inverse
}

