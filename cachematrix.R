## Put comments here that give an overall description of what your
## functions do
## functions that are used to create cached inverse of matrices

## Write a short comment describing this function
##This function is used to create a special object that stores a matrix and cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
#This function calculate the inverse of the special of any matrix that was returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("receiving cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

