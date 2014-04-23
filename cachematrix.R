## Creates a non-singular matrix and stores the inverse if it already exists
## if the inverse does not exist cacheSolve will compute the inverse
## and store it or cache it


makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix establishes a list of functions for a matrix, x
## ARGS:
## 	x: a non-singular matrix
## RETURNS a list of functions pertaining to the values of the matrix
## and its inverse

	n <- NULL

	# set the value of the matrix
	set <- function(z) {
		x <<- z
		n <<- NULL
	}

	# get the value of the matrix
	get <- function() x

	# set the value of the inverse
	setinverse <- function(inverse) n <<- inverse

	# get the value of the inverse
	getinverse <- function() n

	# return list of these functions
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)

} ## end makeCacheMatrix



cacheSolve <- function(x, ...) {
## cacheSolve computes or gets the inverse of the matrix provided
## ARGS:
## 	x: a non-singular matrix
## RETURNS the inverse of x

	# attempt to get the inverse 
    n <- x$getinverse()

	# check for its existence and return if it does exist
    if(!is.null(n)) {
    	message("getting cached inverse")
    	return(n)
    }
    else {
    # compute the inverse of the matrix
    matrix <- x$get()
    n <- round(solve(matrix, ...), 3)
    x$setinverse(n)
    n
	}

} ## end cacheSolve
