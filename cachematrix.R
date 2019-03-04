## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function stores a given matrix x 
## and its inverse after it is computed
## It also returns x and its inverse 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inv <- function(inv) m <<- inv
        get_inv <- function() m
        list(set = set, get = get,
             setinv = set_inv,
             getinv = get_inv)
}


## Write a short comment describing this function
## This function checks if the inverse of x 
## (a variable created with makeCacheMatrix) has been
## cached.
## If it is, it will get the cached data
## If not, it will compute the inverse and store it in x
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
