##cache the value of the inverse so that when we need it again, it can be looked 
#up in the cache rather than recomputed.


#This function, makeCacheMatrix creates a special "matrix", which is really a 
#list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created
#with the above function. However, it first checks to see if the inverse has 
#already been calculated. If so, it gets the inverse from the cache and skips 
#the computation. Otherwise, it calculates the inverse of the data and sets 
#the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data,...)
        x$setinverse(I)
        I
}
