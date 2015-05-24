## makeCaheMatrix function returns a list containing the following functions:
##set: Set the matrix
##get: get the matrix
##setinverse: set the inverse of the matrix
##getinverse: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  

}


## cachesolve function finds the inverse of the matrix created by the above 
## function.It first checks whether the inverse is already calculated. If yes,
## it will retrun the inverse from cache. If not, the inverse is computed and then
## store value if inverse using setinverse function

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
