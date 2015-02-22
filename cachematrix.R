## This script have two functions. makeCacheMatrix and cacheSolve.
## makeCacheMatrix is saving the matrix and its solved results and return it to the cacheSolve function whenever necessry.
## cacheSolve will give the inverse of matrix either by solving the matrix, or giving the cached results if exists. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL              
	set <- function(y) {          
    x <<- y
    inv <<- NULL
	}
    
	get <- function() x        
	setinv <- function(inv) inv <<- inv   
	getinv <- function() inv  
	# available list of methods
	list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv
	)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	#looking into the chached results
    inv <- x$getinv()  
	# returning cached result
	if(!is.null(inv)) {   
		message("getting cached data")
		return(inv)
	}
	# don't have cached results solve the matrix inverse save it to chache and return results
	data <- x$get()         
	inv <- solve(data)
	x$setinv(inv)   
	inv	
}
