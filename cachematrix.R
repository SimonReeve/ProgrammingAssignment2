## This R file is my first file presented in github, 
## whose function is to cache a inverse matrix of one specific matrix

## The following function named makeCacheMatrix creates a specific "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x               #get the spcific matrix
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,        #set the name of function
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function named cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
    m <- x$getInverse()               # Return a matrix that is the inverse of 'x'
    if(!is.null(m))                   # If there exists a !NULL m, return the cached matrix
    {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m    <- solve(data, ...)          # calc. the inverse matrix of m
    x$setInverse(m)
    m                         # return m
}


## Run the above two functions with following commands to test this R file
## > source("cachematrix.R")
## > set.seed(1)       # help to set one specific test matrix
## > A <- makeCacheMatrix(matrix(rnorm(16),4,4))
## > A$get()
##            [,1]       [,2]       [,3]        [,4]
## [1,] -0.6264538  0.3295078  0.5757814 -0.62124058
## [2,]  0.1836433 -0.8204684 -0.3053884 -2.21469989
## [3,] -0.8356286  0.4874291  1.5117812  1.12493092
## [4,]  1.5952808  0.7383247  0.3898432 -0.04493361
## > A$getInverse()    # Return the inverse matrix after running solve(...)
## NULL
## > cacheSolve(A)
##            [,1]       [,2]       [,3]        [,4]
## [1,] -0.6213910  0.2532768  0.1732266  0.44441565
## [2,]  1.5301385 -0.8532288 -0.8240908  0.26742439
## [3,] -0.4197372  0.5574457  0.8748252  0.22924604
## [4,] -0.5605101 -0.1913025  0.1990296 -0.09383139
## > A$getInverse()    # Now we get the inverse matrix
##            [,1]       [,2]       [,3]        [,4]
## [1,] -0.6213910  0.2532768  0.1732266  0.44441565
## [2,]  1.5301385 -0.8532288 -0.8240908  0.26742439
## [3,] -0.4197372  0.5574457  0.8748252  0.22924604
## [4,] -0.5605101 -0.1913025  0.1990296 -0.09383139
## > cacheSolve(A)     # Cached data expected
## getting cached data
##            [,1]       [,2]       [,3]        [,4]
## [1,] -0.6213910  0.2532768  0.1732266  0.44441565
## [2,]  1.5301385 -0.8532288 -0.8240908  0.26742439
## [3,] -0.4197372  0.5574457  0.8748252  0.22924604
## [4,] -0.5605101 -0.1913025  0.1990296 -0.09383139
## > A <- makeCacheMatrix(matrix(c(1,2,-1,-1),2,2))
## > A$get()
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
## > cacheSolve(A)
##      [,1] [,2]
## [1,]   -1    1
## [2,]   -2    1
## Following commands will produce ridiculous results.
## > A <- makeCacheMatrix(matrix(c(1,2,-1,-1),2,2))
## > A$setInverse(0)  # Don't manually set any data for function setInverse()
