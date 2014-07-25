## For any matrix, x, create a new "matrix" object
## that can cache its inverse with makeCacheMatrix
## then use cacheSolve on the new "matrix" object

## Set the value of a matrix and cache its inverse
## plus functions to retrieve the matrix and cached inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Check to see if the inverse has already been calculated
## If so, then get the inverse from the cache
## If not, then solve for inverse and set it in the cache via setinv

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
