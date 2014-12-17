## This R file is my first file presented in github, 
## whose function is to cache a inverse matrix of one specific matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatri <- function(x = numeric())
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatri above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
    m <- x$getInverse()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m    <- solve(data, ...)
    x$setInverse(m)
    m
}


## Run the above two functions with following commands to test this R file
## source("cachematrix.R")
## set.seed(1)
## A <- makeCacheMatri(matrix(rnorm(16),4,4))
## A$get()
## A$getInverse()
## cacheSolve(A)
## A$getInverse()
## cacheSolve(A)
