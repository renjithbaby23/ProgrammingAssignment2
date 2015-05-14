## These function together retrives the value of the Matrix inverse from the cache if it is already calculated.
## If it is not already calculated, these functions saves the inverse which it is calculating on a variable in cache


## Creates a special Matrix for the 'cacheSolve' to work on. Actually it contains a list of functions to
## access or modify the value of the input Matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	invA<-NULL
	set <- function(A) {
                x <<- A
                invA <<- NULL
	}
	get <- function() x
	setinv <- function(solve) invA <<- solve
	getinv <- function() invA
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)

}


## Gets the inverse of the matrix if it already exists in the chache variable.
## If it doesn't exist, then calculates the inverse and stores it in a cache variable.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invA <- x$getinv()
		if(!is.null(invA)) {
			message("getting cached Matrix...")
			return(invA)
		}
	data <- x$get()
	invA <- solve(data)
	x$setinv(invA)
	invA
}
