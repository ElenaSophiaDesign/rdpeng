##The objetives of this code are
##Return a matrix that is the inverse of 'x'
##Explain the code 

## This funtion creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## The process initialize the inverse property
	## and the identity matrix
    i <- NULL
	identityM <- NULL
	
	## Build identity matrix of the same dimensions as your input matrix 
	d <- nrow(m)
	identityM <- diag(d)
	
	## Process to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Process the get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Process to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Process to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the processes facts
	## including identity matrix of the same dimensions as the input matrix
	
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse,
		 identityM = identityM )
}


## Compute the inverse of the matrix above 
cacheSolve <- function(x, ...) {	

    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()

    ## Return the inverse if its already set
    if( !is.null(i) ) {
            message("getting cached data")
            return(i)
    }

    ## Get the matrix from the data 
    data <- x$get()
	
	## get the identity matrix corresponding to the data
	identityM <- x$identityM 
	
    ## Use matrix multiplication to get the inverse 

    i <- solve(data) %*% identityM

    ## Set the inverse
    x$setInverse(i)

    ## Print the matrix
	print (i)
	
	}

	
	
