## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function defines the value of the Matrix & Inverse Matrix.
## It also returns functions to assign Matrix, generate and assign Inverse Matrix, 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL				## Initialise Inverse as NULL
	set <- function(y)	{	## Input and assign Matrix to Global Env
		x <<- y
		i <<- NULL			## Assign Inverse in Global Env
	}
	
	get <- function()			## Returns Matrix	
		{x}
	
	setinverse <- function(inverse)	## Assign Inverse Matrix
		{i <<- inverse}
	
	getinverse <- function()	## Return Inverse Matrix
		{i}
	
	list(set = set, 				##Return List of Functions that will be called by cacheSolve
		get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

## cacheSolve takes a Matrix as argument, and checks if the Inverse is stored.
## If inverse already calculated, will return cached value, else calculate and assigns inverse

cacheSolve <- function(x) {
	i <- x$getinverse()		
	if(!is.null(i)) {			##Checks for Inverse value if stored
                message("getting cached data")
                return(i)		## Return inverse of Matrix from Cache
	}
	i <- solve(x$get())		## Generates and assigns inverse matrix
	x$setinverse(i)
      return(i)				## Return inverse of Matrix	
}