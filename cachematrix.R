## Put comments here that give an overall description of what 
## your functions do
##The following pair of functions cache the inverse of a 
##matrix x


## Write a short comment describing this function
##makeCacheMatrix creates a special "matrix" which contains
##a function to
## 1) set the value of the matrix x
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {


	inv <- NULL
	set <- function(y) {
		x <<- y      
		inv <<- NULL
	}
	get <- function() x
	setinverse<- function(inverse)inv <<-inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## Write a short comment describing this function

##cacheSolve calculates the inverse of matrix x. It first check
##to see if the inverse has been calculated 
## if yes, cacheSolve gets the inverse from the cache
## if no, cachSolve calculates the inverse

cacheSolve <- function(x, ...) {
       
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("Getting cached data.")
		return(inv)
	}
		data <- x$get()
		inv <- solve(data)
		x$setinverse(inv)
		inv
}
