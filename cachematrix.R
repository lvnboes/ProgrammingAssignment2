## First function accepts a matrix and returns a list of functions to make both it and its invers cachable.
## the second function checks for cached values first and if they don't exist yet, calculates the inverse and caches it.

## This function takes a matrix and creates and returns a list of getters and setters to save/retrieve the matrix and its inverse to/from variables in the global environment

makeCacheMatrix <- function(x = matrix()) {
	inverse_matrix <- NULL
	set_matrix <- function(y) {
		x <<- y
		inverse_matrix <<- NULL
	}
	get_matrix <- function(){
		return(x)
	}
	set_inverse <- function(inverse){
		inverse_matrix <<- inverse
	}
	get_inverse <- function(){
		return(inverse_matrix)
	}
	return(list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse))
}


## This function accepts the list created by the previous function and uses it to check if the inverse has already been calculated and set to the global environment. 
## If it has, it returns the existing value, if not, it calculates the inverse, sets the global variable to avoid unnecessary future calculations and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
        if(!is.null(inverse)){
        	return(inverse)
        }
        inverse <- solve(x$get_matrix(),...)
        x$set_inverse(inverse)
        return(inverse)
}
