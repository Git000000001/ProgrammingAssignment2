## These functions relate to calculate the inverse of a matrix.  However, they
## skip the calculation if the value of the matrix inverse is saved in the cache.

## Creates a special "matrix", which is a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Calculates the invesrs of the special "matrix" created in makeCacheMatrix.
##  However, it first checks if the inverse has already been calculated.
##  If so, it gets the inverst from the cache and skips the calculation.
##  Otherwise, it calculates the inverse and sets the value of the inverst in the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv		
}
