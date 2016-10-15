## Make object with cache inverse matrix and method get, set, getinverse and setinverse
makeCacheMatrix <- function(x = matrix()) {

	inverse <-NULL
	dimen = dim(x)
	if(is.null(dimen)){
		warning("Matrix invalid")
	}else if(dimen[1]!=dimen[2]){
		warning(paste("Matrix Num Row", dimen[1] , " Num Col", dimen[2]))
	}
	set <- function(y) {
		x <<-y
		inverse <<- NULL
	}
	#set(x)
	get <- function() x
	getinverse <- function() inverse
	setinverse <- function(invers) inverse <<- invers
	list(set = set, get = get, 
		getinverse = getinverse, setinverse = setinverse)
}

## Return inverse matrix using cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
    if(!is.null(inverse)) {
     	message("getting cached data")
        return(inverse)
     }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse )
    inverse 
}
