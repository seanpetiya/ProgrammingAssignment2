## This pair of functions uses lexical scoping to cache matrix inversions.

## The makeCacheMatrix function creates a matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() {
		x
	}
	setInverse <- function(inverse) {
		i <<- inverse
	}
	getInverse <- function() {
		i
	}
	list(set = set,get = get, 
		setInverse = setInverse,
		getInverse = getInverse)
}


## The cacheInverse function caches and retrieves the inverse of a matrix.

cacheInverse <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i)) {
		message("Getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
