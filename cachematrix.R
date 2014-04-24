## Following pair of functions compute and cache the inverse of a matrix rather than repeatedly computing it

## makeCachematrix creates a special "matrix" object that caches its inverse. 
## Input to this function is a matrix. 
## Output is a special "matrix" that is a list containing a function to
##	1. Set the value of the matrix	
##	2. Get the value of the matrix
##	3. Set the value of the matrix inverse
##	4. Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

		i <- NULL
		set <- function(y) {
				x <<- y
				i <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) i <<- inverse
		getinverse <- function() i
		## Return a list containing functions to set & get the matrix and set and get the inverse of the matrix
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
	   
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		i1 <- x$getinverse()
		if(!is.null(i1)) {
				message("getting cached data")
				return(i1)
		}
		data <- x$get()
		i1 <- solve(data, ...)
		x$setinverse(i1)
		## Return a matrix that is the inverse of 'x'
		i1
}
