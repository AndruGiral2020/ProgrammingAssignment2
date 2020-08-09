## Put comments here that give an overall description of what your
## functions do

## The first function, makeVector creates a special "matrix", 
## which is really a list containing a function 


makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y){
        x <<- y
        j <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## The following function calculates the mean of the special "matrix" 
## created with the above function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    j <- x$getInverse()
    if(!is.null(j)){
        message("getting cached data")
        return(j)
    }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
    j
}


m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
