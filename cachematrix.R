## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.


## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(Y) {
        x <<- Y
        I <<- NULL
    }
    get <- function() x
    setinv <- function(solve) I <<- solve
    getinv <- function() I
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function calculates the inverse of the matrix created with the above function and stores it in cache.
## If the inverse has already been calculated it gets the mean from the cache and skips the computation.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    I <- x$getinv()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data)
    x$setinv(I)
    I
}
