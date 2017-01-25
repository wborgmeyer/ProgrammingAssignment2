## These functions are used to calculate the inverse of a matrix
## it is assumed that the matrix is invertable
## if the inverse is already cached then the calculation is not repeated
## the function makeCacheMatrix is used to cache the inverse of the matrix

## makeCacheMatrix is a function which contains a list of embedded functions
## set() sets the matrix value and unsets the inverse (i)
## get() returns the value of the matrix
## setinverse()  sets the inverse value
## getinverse()  returns the inverse value

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a function of class makeCacheMatrix and calculates the inverse
## if the inverse has already been calculated it simply takes the calculated value
## by calling getinverse which is embedded in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached inverse")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
      
}
