## This function creates a wrapper object for "matrix"  that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(val) { x <<- val; inv <<- NULL }
    get <- function() x
    set_solve <- function(val) inv <<- val
    get_solve <- function() inv
    list(set = set, get = get, set_solve = set_solve, get_solve = get_solve)
}


## This function computes the inverse of a wrapped "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and
# the matrix has not changed), then cacheSolve fetchs the inverse
# from the cache
cacheSolve <- function(x, ...) {
    inv <- x$get_solve()
    if (!is.null(inv)) {
        message("getting cached data")
    } else {
        data <- x$get()
        inv <- solve(data, ...)
        x$set_solve(inv)
    }
    inv
}

