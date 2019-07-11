## Jacob Segal
## Programming Assignment 2
## Lexical Scoping
## These functions cache the inverse of a matrix (assuming matrix is invertible)



## makeCacheMatrix creates a matrix that can cache its invers

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the cached matrix from the previous function
## If the inverse has already been calculated, cacheSolve retrieves it from the cache

cacheSolve <- function(x, ...)
{
  m <- x$getInverse()
  if (!is.null(m))
  {
    message("retrieving cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  return(m)
}
