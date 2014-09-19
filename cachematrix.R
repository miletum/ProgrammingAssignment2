## This function creates a special "matrix" object that can cache its inverse.


## It is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of 'x'
## 4. get the value of the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setCacheMatrix <- function(Matrix) m <<- Matrix
      getCacheMatrix <- function() m
      list(set = set, get = get,
           setCacheMatrix = setCacheMatrix,
           getCacheMatrix = getCacheMatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      m <- x$getCacheMatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setCacheMatrix(m)
      m
## Return a matrix that is the inverse of 'x'
}
