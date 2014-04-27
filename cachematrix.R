## This function creates a wrapper object for a matrix" x  that can cache its own inverse
#      Param [x]: A Matrix that you want to be able to cache its inverse
#      Return : An object that can be used with cacheSolve function to calculate
#               and cache the inverse of matrix x
#
# EXAMPLE:
# invertible_m = matrix( c(1,2,3,1,4,9,1,8,27))  
# wrapped_m = makeCacheMatrix( invertible_m )
#
# now wrapped_m is an object that can handle its own inverse with these properies:
#
# wrapped_m$get() == invertible_m
# cacheSolve( wrapped_m ) == solve( invertible_m ) 
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # Local function used to handle the inverse of wrapped
    set <- function(val) { x <<- val; inv <<- NULL }
    get <- function() x
    set_solve <- function(val) inv <<- val
    get_solve <- function() inv
    list(set = set, get = get, set_solve = set_solve, get_solve = get_solve)
}


## This function computes the inverse of a wrapped "matrix" returned by 
# makeCacheMatrix above. It works this way: 
# If the inverse has already been calculated (and the matrix has not changed), 
# then this matrix is fetched from the cache, else it was calculated, cached and
# returned as result
#
#      Param x: a wrapped matrix object returned from makeCacheMatrix
#      result : the inverse of the matrix wrapped inside x (x$get())
##
cacheSolve <- function(x, ...) {
    inv <- x$get_solve()
    if (!is.null(inv)) {
        message("getting cached data")
    } else {
        w_matrix <- x$get()
        inv <- solve(w_matrix, ...)
        x$set_solve(inv)
    }
    inv
}

