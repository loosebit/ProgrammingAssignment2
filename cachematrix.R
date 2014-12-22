
## Creates a special "matrix", which is really a list containing functions
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix
## This function captures value of the parameter passed in (x) and defines an local variable (xinv)
## Both can be modified by calling set(), which affects variables in its parent environment.

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
      x <<- y
      xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Takes a special "matrix" (created by a call to makeCacheMatrix()) as a parameter.
## Checks whether an inverse has already been calculated.
## Returns cached value if it has; otherwise inverts the matrix, and caches and returns the result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    xinv <- x$getinv()
    if(!is.null(xinv)) {
      message("getting cached data")
      return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}
