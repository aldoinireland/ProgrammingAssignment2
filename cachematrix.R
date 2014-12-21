
## makeCacheMatrix creates a list of 4 functions which
##  store a matrix (set)
##  return the stored matrix (get)
##  take and store/cache the inverse of a matrix (setInverse)
##  return the cached inverse of the original matrix (getInverse)
##  pair the functions with tags so they can be called externally (list)
##

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Store a matrix passed into the function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Get the stored matrix to return
    get <- function() x
    ## Store the inverse 
    setInverse <- function(solve) m <<- solve
    ## Get the stored inverse to return
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##  This function uses the capabilities created using 
##      makeCacheMatrix to either get the inverse of the
##      original matrix, store and return it, or retrieve
##      and return it if it was already cached.
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' if it's available
    m <- x$getInverse()
    ## Check to see if it was available - if so, issue message and 
    ##      return it
    if(!is.null(m)) {
        message("Returning cached data please be patient")
        return(m)
    }
    ## Otherwise .... 
    ##  ... get the original matrix ...
            message("No Cached Results Calculating Inverse matrix now")
			data <- x$get()
    ##  ... derive the inverse ...
    m <- solve(data, ...)
    ##  ... store it ...
    x$setInverse(m)
    ##  ... and return it.
    m

}

## Example below for an output
### Create a matrix
TEST_1 <- makeCacheMatrix(matrix(1:4,2))
###Get this result
TEST_1$get()
###Get the inverse
TEST_1$getInverse()
##reset the matrix
TEST_1$set(matrix(5:8,2))
##Get this result
TEST_1$get()
##Inital test non cached
cacheSolve(TEST_1)
#Return cached results Or it should
cacheSolve(TEST_1)
##test the inverse
TEST_1$getInverse()
b = TEST_1$getInverse()
TEST_1$get() %*% b