## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create a special "matrix" object cache x and its inverse, 
## which really is a list containing functions:
## 1. get the value of the matrix
## 2. set the value of the matrix
## 3. get the inverse matrix
## 4. set the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get, 
		setInverse = setInverse, 
		getInverse = getInverse)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
## If the inverse has been calculated, return it,
## else calculate and cache the inverse, and return it.
cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if (!is.null(i)) return(i)	## Already calculated

	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}

