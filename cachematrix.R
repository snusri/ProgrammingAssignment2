#By snusri

# The function makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
       
                invrs <- NULL
                set <- function(y) {
                        x <<- y
                        invrs <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) invrs <<- inverse
                getinverse <- function() invrs
                     list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse= getinverse)
        
}

##It should be noted in the following function, cacheSolve, the matrix should always be invertible i.e, 
## matrix should be square and none of the rows or columns should be zero

cacheSolve <- function(x, ...) {
                invrs <- x$getinverse()
                if(!is.null(invrs)) {
                        message("getting cached data")
                        return(invrs)
                }
                data <- x$get()
                invrs <- solve(data)
                x$setinverse(invrs)
                invrs
        
}
