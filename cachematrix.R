## Your assignment is to write a pair of functions that cache 
## the inverse of a matrix.

## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.

## First ,the cache value is created and set to "NULL". Then the matric is 
## created and inverted. It's then store in cache and then retrieved 


makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  get <- function() x
  setMatrix <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  
  list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse)
  
## `cacheSolve`: This function computes the inverse of the special "matrix"
## returned by `makeCacheMatrix` above. If the inverse has already been 
## calculated (and the matrix has not changed), then `cacheSolve` should 
## retrieve the inverse from the cache.Code assumes the matrix is invertible,
## geting the inverted matrix from cache if it exists and display a message
## saying data is being retrieved and then dsiplaying the result.
  
  
  cacheSolve <- function(x, ...) {
    
    cache <- x$getInverse()
    
    if (!is.null(cache)) {
      message("getting data")
      return(cache)
    }
    
    matrix <- x$get()
    
    tryCatch( {
      cache <- solve(matrix, ...)
    },
    matrixupdated = {
      x$setMatrix(cache)
    } )
    
    return (cache)
  }
  