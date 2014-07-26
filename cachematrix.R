## In order to save time in computing and re-computing large quantities of data 
## we use caches to store data that may be re-used. 
## The two functions below will be used to create a space in order to cache the inverse of a matrix
## and output the inverse without the need for calculation, if the inverse has not changed
## If the inverse has changed, the new inverse will be calculated and output
##

## The makeCacheMatrix function is created in order to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
    



## The cacheSolve function first checks to see if the inverse of the 
## matrix has already been calculated, if so then it gets the inverse 
## from the cache, if not, it calculates the inverse

cacheSolve <- function(x, ...) {

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## The End

