## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix stores the Inverse Matrix value as cache and generates a list as output
## The "<<" operator uses lexical scoping of R and changes x and I values in parent environment
makeCacheMatrix <- function(x = matrix()) {
I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(Inv) I <<- Inv
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##Cachesolve takes makecachematrix as an argument
##Checks if cache value is present in makecachematrix function. if not it generates an inverse and stores in setinverse function

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
  if(!is.null(I)) {
    print("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I}
