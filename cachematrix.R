## The functions will cache the inverse of matrix

## This function creates a "matrix" which is a list containing a function

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y){
                            x <<- y
                            i <<- NULL
                }
                get <- function()x
                setinv <- function(inverse) i <<- inverse
                getinv <- function() i
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
          # get the inverse of the matrix and store in inverse variabe
          i <- x$getinv()
          # test if matrix existed from cache
          if(!is.null(i)) {
                          message("getting cached data")
                          return(i)
          }
          data <- x$get() # create the data matrix
          
          i <- solve(data) %*% data #try to get the inverse of the matrix
          x$setcache(i)
          i
  }