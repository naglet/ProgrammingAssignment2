## Coursera | rprog-010 | Assignment 2
## Use caching to get the inverse of a matrix

## Inverting a matrix can be long, want to use cache when available
## Assume the matrix is always invertible; use the solve() function

makeCacheMatrix <- function(x = matrix()) {

## This function creates a special "matrix" object that can cache its inverse.
        
        mx_inv <- NULL
        set_mx <- function(y){
                x <<- y
                mx_inv <<- NULL
        }
        
        get_mx <- function() x
        
        set_mx_inv <- function(solve) mx_inv <<- solve
        get_mx_inv <- function() mx_inv
        
        list(set_mx = set_mx, 
             get_mx = get_mx, 
             set_mx_inv = set_mx_inv, 
             get_mx_inv = get_mx_inv)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        mx_inv <- x$get_mx_inv()
        
        ## check solution is in cache
        if(!is.null(mx_inv)){
                        message("getting cached data")
                        return(mx_inv)
                        }
        
        ## get inverse if not in cache
        data <- x$get_mx()
        mx_inv <- solve(data, ...)
        x$set_mx_inv(mx_inv)
        
        ## return the inverted matrix:
        mx_inv
}

## solution works when run as: 
## > cacheSolve(makeCacheMatrix(x))
