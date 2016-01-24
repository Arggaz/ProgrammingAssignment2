## Assignment #2
## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a "matrix" object
## that can cache its inverse. The Matrix contains a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inversed matrix
## get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

## Execution example ran for the assignment demonstration
## x = matrix(c(2, 4, 3, 1, 5, 7,8,6,7),nrow=3,ncol=3,byrow = TRUE)
## > x
## [,1] [,2] [,3]
## [1,]    2    4    3
## [2,]    1    5    7
## [3,]    8    6    7
## m = makeCacheMatrix(x)

## m$get()
##[,1] [,2] [,3]
## [1,]    2    4    3
## [2,]    1    5    7
## [3,]    8    6    7

## first run with empty cache
## > cacheSolve(m)
## [,1]   [,2]    [,3]
## [1,] -0.0875 -0.125  0.1625
## [2,]  0.6125 -0.125 -0.1375
## [3,] -0.4250  0.250  0.0750

## second run with data already in cache
## > cacheSolve(m)
## getting cached data
## [,1]   [,2]    [,3]
## [1,] -0.0875 -0.125  0.1625
## [2,]  0.6125 -0.125 -0.1375
## [3,] -0.4250  0.250  0.0750
