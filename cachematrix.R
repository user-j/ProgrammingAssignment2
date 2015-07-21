@@ -1,15 +1,44 @@
## The following two functions first makes a special object that stores an invertible matrix
## and secondly caches (and returns) the inverse of that matrix.

## Write a short comment describing this function
## The following fuction, makeCacheMatrix, creates a list of functions to:
##	1) Set:  store an invertible matrix in memory.
##	2) Get:  retrieve the matrix for use in the functions.
##	3) SetInverse:  store the inverse of the matrix in memory.
##	4) GetInverse:  retrieves the data stored in memory (NULL or not).

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## Write a short comment describing this function
## The following function, cacheSolve, checks to see if the inverse of a matrix has been calculated
## using getinverse().  If it has, then cacheSolve returns the inverse of the matrix.
## If getinvsere() is null, then it calculates and returns the inverse of the matrix and stores
## the inverse in the cache for possible future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
## Return a matrix that is the inverse of 'x'	

	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
        
}
