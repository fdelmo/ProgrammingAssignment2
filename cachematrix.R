## makeCacheMatrix takes a matrix type argument and return a special
## matrix object. cacheSolve calculates the inverse of the makeCacheMatrix
##object and if it has been calculated before it returns the value from the cache.

## makeCacheMatrix create as special matrix object.

makeCacheMatrix <- function(x = matrix()) {
    elc <- NULL
    set <- function(y) {
        x <<- y
        elc <<- NULL
    }
    get <- function() x
    setreverse<- function(reverse) elc <<-reverse
    getreverse <- function() elc
    list(set = set, get = get,
         setreverse = setreverse,
         getreverse = getreverse)
}
## Return a matrix that is the inverse of 'x'. If this matrix has been
##calculated before, it will be returned form the cache without calculate
##it again.

cacheSolve <- function(x, ...) {
    elc <- x$getreverse()
    if (!is.null(elc)) {
        message("getting cached reververse matrix")
        return(elc)
    } else {
        elc <- solve(x$get())
        x$setreverse(elc)
        return(elc)
    }
}
