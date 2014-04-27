## This function creates a wrapper object for a matrix" x  that can 
# cache its own inverse,
#      Param [x]: A Matrix which you want to compute & cache its inverse
#      Return : An object that can be used with cacheSolve function to 
#               calculate and cache the inverse of matrix x
#
# EXAMPLE:
# invertible_m = matrix( c(1,2,3,1,4,9,1,8,27))  
# wrapped_m = makeCacheMatrix( invertible_m )
#
# wrapped_m is an object that has this two properties:
# 1) wrapped_m$get() == invertible_m
# 2) cacheSolve( wrapped_m ) == solve( invertible_m ) 
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Local functions used to handle the inverse of x thorough cache
    set <- function(val) { 
        x <<- val 
        inv <<- NULL 
    }
    get <- function() x
    set_inv <- function(val) inv <<- val
    get_inv <- function() inv
    
    # Return a list with inv. function handlers
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function computes the inverse of a wrapped "matrix" returned 
# by makeCacheMatrix above. It works this way: 
# If the inverse has already been calculated (and its matrix has not 
# changed), then this matrix is fetched from the cache, else it was 
# calculated, cached and returned as result
#
#      Param x: a wrapped matrix object returned from makeCacheMatrix
#      result : the inverse of the matrix wrapped inside x (x$get())
##
cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    if (!is.null(inv)) { # Cache found, no need to re-compute
        message("getting cached data")
    } else { # No cache found, compute-cache the inverse of x
        w_matrix <- x$get()
        inv <- solve(w_matrix, ...)
        x$set_inv(inv)
    }
    inv
}

