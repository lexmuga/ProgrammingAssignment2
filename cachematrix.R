## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function, makeCacheMatrix, creates a special "matrix", 
## which we assume to be invertible and
## which is really a list containing a function to
## 1. set the value of the square matrix
## 2. get the value of the square matrix
## 3. set the value of the inverse of the square matrix
## 4. get the value of the inverse of the square matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Write a short comment describing this function
## The function "cacheSolve" calculates the inverse of
## the special "matrix" created with the makeCacheMatrix function.
## It first checks if the inverse of the matrix has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)){
                message("getting cache data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
