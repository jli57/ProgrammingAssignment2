## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix makes a matrix object that can cache its inverse. 
## The matrix object will return a list of functions that can set/get 
## the value of matrix and its inverse in the cache.

## cacheSolve takes the matrix object created by makeCacheMatrix and
## checks if the inverse matrix is already stored in the cache. It
## returns the value of the inverse in the cache if it was computed 
## before. If the inverse is not in the cache, cacheSolve solves for 
## the inverse and stores the value of the inverse in the cache.



## Write a short comment describing this function

## makeCachMatrix creates a matrix object that can cache its inverse.
## Input: An invertible matrix
## Output: A list of functions

makeCacheMatrix <- function(x = matrix()) {
	
	## set the inverse to NULL by default
	inv <- NULL
	
	## define a function to set the value of the matrix in the cache
	## store the inverse as NULL in the cache
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	## define a function to get the value of the matrix from the cache
	get <- function() x
	
	## define a function to set the value of inverse in the cache
	setinv <- function(solve) inv <<- solve

	## define a function to get the inverse from the cache
	getinv <- function() inv

	## returns the four functions in a list as the output 
	list( set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

## cacheSolve computes the inverse of the special matrix returned by 
## makeCacheMatrix. This function will return the value of the inverse
## store in the cache if it exists. Otherwise, it will calculate the 
## value of the inverse matrix and store the inverse matrix in the cache. 

## Input: A special matrix object created with makeCacheMatrix
## Output: The inverse of matrix object

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	## get the value of the inverse in the cache   
	inv <- x$getinv()

	## Check if the value of the inverse is already in the cache
	## return the value of the inverse as stored in the cache
	if(!is.null(inv)) {
		  
		## display a message in the R command line indicating
		## the inverse was retrieved from the cache
		message("getting cached data")  
		    
		## returns the value of the inverse as stored in the cache
		return(inv)			    
	}

  
	## The following statements will be executed if the value of the
	## inverse was not found in the cache
  
	## If the inverse is not in the cache, get the value of the matrix
	data <- x$get()
	
	## compute the inverse of the matrix using the solve function
	inv <- solve(data, ...)

	## set the value of the inverse in the cache to the value that was 
	## computed with the solve function in the previous statement
	x$setinv(inv)

	## return the value of the inverse matrix as the output of this function
	inv
}
