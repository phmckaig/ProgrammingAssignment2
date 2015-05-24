## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
#  rather than compute it repeatedly 
# 
#  The following two functions will be used in order to calculate the inverse of
#  a matrix and cache the result for a subsequent retrieval:

#  1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#  2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



# makeCacheMatrix creates a list of individual functions to
# 1. set the value of the input matrix
# 2. get the value of the input matrix
# 3. set the value of inverse of the input matrix
# 4. get the value of inverse of the input matrix
# In other words, in addition to the list, we have getters and setters
# The end result is a cached version of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ##   init matrix
  matrix <- NULL
  
  ##   setter for input
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  ##   getter for input
  get <- function() x
  ##  setter and getter for inverse
  setInverse <- function(inverse) matrix <<- inverse
  getInverse <- function() matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve takes a cached matrix and first checks to see if the inverse
#  of the matrix exists. If it exists, it returns it; if not, it calculates the inverse of the
#  matrix, caches the inverse, and returns the computed inverse.
#  Note that this function assumes that the input matrix is invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  matrix <- x$getInverse()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  ## compute inverse
  inverse <- solve(data, ...)
  ## cache inverse
  x$setInverse(inverse)
  
  inverse
  
  
}

##TESTS

## > source("cachematrix.R")
## > y = rbind(c(1, -1/2), c(-1/2, 1))
## > m = makeCacheMatrix(y)
## > m$get()
## [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0
## > m$getInverse()
## NULL
## > cacheSolve(m)
## [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## > m$get()
## [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0
## > m$getInverse()
## [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## > y = rbind(c(1, 0), c(0, 1))
## > m = makeCacheMatrix(y)
## > cacheSolve(m)
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > x = rbind(c(1, 2, -4), c(-1, -1, 5),c(2,7,-3))
## > m$get()
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2] [,3]
## [1,]    1    2   -4
## [2,]   -1   -1    5
## [3,]    2    7   -3
## > cacheSolve(m)
## [,1]  [,2] [,3]
## [1,] -16.0 -11.0  3.0
## [2,]   3.5   2.5 -0.5
## [3,]  -2.5  -1.5  0.5
