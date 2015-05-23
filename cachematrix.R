##The objetives of this code are
##Return a matrix that is the inverse of 'x'
##Explain the code 

## This funtion creates a special matrix object that can cache its inverse
+makeCacheMatrix <- function( m = matrix() ) {
+
+	## The process initialize the inverse property
+    i <- NULL
+
+    ## Process to set the matrix
+    set <- function( matrix ) {
+            m <<- matrix
+            i <<- NULL
+    }
+
+    ## Process the get the matrix
+    get <- function() {
+    	## Return the matrix
+    	m
+    }
+
+    ## Process to set the inverse of the matrix
+    setInverse <- function(inverse) {
+        i <<- inverse
+    }
+
+    ## Process to get the inverse of the matrix
+    getInverse <- function() {
+        ## Return the inverse property
+        i
+    }
+
+    ## Return a list of the processes facts
+    list(set = set, get = get,
+         setInverse = setInverse,
+         getInverse = getInverse)
+}
+
+
+## Compute the inverse of the matrix above 
+cacheSolve <- function(x, ...) {
+
+    ## Return a matrix that is the inverse of 'x'
+    m <- x$getInverse()
+
+    ## Return the inverse if its already set
+    if( !is.null(m) ) {
+            message("getting cached data")
+            return(m)
+    }
+
+    ## Get the matrix from the data 
+    data <- x$get()
+
+    ## Use matrix multiplication to get the inverse 
+    m <- solve(data) %*% data
+
+    ## Set the inverse
+    x$setInverse(m)
+
+    ## Print the matrix
