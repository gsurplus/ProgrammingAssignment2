## These functions assist with creating a special matrix, computing its
## inverse, and caching it so that it can be retrieved again without having
## to waste computation time in calculating it again.


##This function creates a special "matrix" object that can cache its
##inverse
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
     x <<- y
     i <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) i <<- inverse
   getInverse <- function() i
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
   
}


##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above.  If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve
##the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   i <- x$getInverse()
   if(!is.null(i)){
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data,...)
   x$setInverse(i)
   i
}
