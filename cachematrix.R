## makeCacheMatrix takes a matrix as an argument, and returns functions for setting and getting
## data, to and from the matrix and its inverse. cacheSolve checks if the matrix returned from
## makeCacheMatrix has an inverse value already calculated. If the inverse is already calculated,
## it loads the value from the cache, and if it isn't calculated, it solves it, and then adds the
## new value to the cache, and in either case, returns the matrix.

## makeCacheMatrix creates a matrix object, and returns a list of all the functions
## that can be used to get and set data of the matrix and it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(inverse) i <<- inverse
	
	getInverse <- function() i
	
	list(	
		set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
}

## cacheSolve returns the inverse of the matrix, either by solving it, or by getting it from the 
## cache.

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	
	if( !is.null(i) ) {
		message("getting cached data")
		return(i)
	}
	
	data <- x$get()
	
	i <- solve(data, ...)
	
	x$setInverse(i)
	
	i
}
