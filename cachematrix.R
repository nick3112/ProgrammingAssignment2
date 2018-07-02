#cache matrix and inverse to save coding expense

##first: create the matrix.  run the below code to create an invertible matrix
##and..: to run the functions in order to get the matrix inversion (calculated or cached)
#> x<-matrix(c(0.5,-1,-0.25,0.75),ncol=2,nrow=2)
#> m<-makeCacheMatrix(x)
#> cacheSolve(m)

## now create makeCacheMatrix; 
	#this will store the matrix and calculate/cache the inverse too
	#using lexical scoping, the store will be the function memory
	#this avoids messy use of globals which can be over-written by other functions

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL  #reset the variable to store the inverse of the matrix
	set <- function(y) {  #create a function which will create the function memory needed to cache
		x <<- y  	#store the original variable with the matrix
		m <<- NULL 	#store the created variable with the inverted matrix
	}
	get <- function() x	#create get function terminology for next lines

	#get and set the inverse
	setinverse <- function(solve) m<<-solve
	getinverse <- function() m

	#create list for future use in next main function cacheSolve
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


##this function checks to see if the matrix has changed and if the inverted matrix exists
	# if the matrix has changed then it will recalculate the solution
	# if the matrix is the same, it will pick up from the cache

	## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      m <- x$getinverse() #get the function 'inverse' from the prior list

	#if the below is true, we have found our input data and can simply retrieve the solution
	if(!is.null(m)) {  
		message("getting cached solution")
		return(m)
	}

	#otherwise, go back and get the solution/inverse by the original function (from list)
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)

	#output the final inverted matrix
	m
}
