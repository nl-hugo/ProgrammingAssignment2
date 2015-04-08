## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following functions cache the inverse of a matrix.

## Creates a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'. If the inverse is already in the
## cache, it returns the cached matrix. If not, it computes the inverse matrix
## and stores it in the cache. This function assumes 'x' to be an invertible
## (i.e. n-by-n sized) matrix.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Test:
## > x = matrix( c(2, 4, 3, 1), nrow=2, ncol=2) 
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    2    3
## [2,]    4    1

## First run: no cached matrix
## > cacheSolve(m)
## [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2

## Next run: matrix is cached
## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2
