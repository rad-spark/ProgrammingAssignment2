## Caching the inverse of a matrix


## This function returns a list of 4 functions
##
## set        - set the value of the matrix
## get        - get the value of the matrix
## setinverse - set the inverse of the matrix
## getinverse - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve function expects an argument (x) which is the list
## returned by the makeCacheMatrix function.
##
## It returns the inverse of the matrix if it already computed and
## cached otherwise it computes the inverse of the matrix and saves it
## in the cache using setinverse and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
		return(inv)
        }
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
        inv
}
