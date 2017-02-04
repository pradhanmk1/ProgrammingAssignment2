## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly . Below functions are  there to cache the inverse of a matrix.

## makeCacheMatrix creates a special "Matrix that can cache its inverse", 
## which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
        set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
	        return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	return (inv)
}
