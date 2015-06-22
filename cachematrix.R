## Caching and Inverse of a Matrix 
## Below is a pair of functions that cache the inverse of a matrix.
## Two functions makeCacheMatrix & cacheSolve have been defined below

## makeCacheMatrix function Description: 
## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) m <<- inverse
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function Description:
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. 
## This function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips
## the computation. If not, it computes the inverse, sets the value in the cache
## via setinv

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- inverse(data, ...)
	x$setinv(m)
	m
}
