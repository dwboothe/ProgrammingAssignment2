## Dwayne Boothe

## makeCacheMatrix returns a special 'matrix' (makeCacheMatrix) provided a square matrix, x
## which provides an interface for x.
## cacheSolve returns the inverse value of a 'special' matrix (cacheMatrix), x 
## if the value is already computed/cached it is returned overwise computed,
## cached and returned.


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # set inverse matrix variable to NULL
    x_inverse <- NULL
    
    # set function to set value of matrix x from called environment
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    
    # get function returns the current matrix value
    get <- function() x
    
    # getinverse function returns the inverse value
    getinverse <- function() x_inverse
    
    # setinverse function sets the inverse matrix value 
    setinverse <- function(inverse) x_inverse <<- inverse
    
    # return list object of the above defined function
    list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    # get the current value of the inverse of 'x'
    x_inverse <- x$getinverse()
    
    # checks if there is a cached value of the inverse of 'x' 
    # and returns it if not null
    if(!is.null(x_inverse)) {
        message("getting cached data")
        return(x_inverse)
    }
    
    # get the value of the matrix 'x'
    data <- x$get()
    
    # calculate the inverse of the matrix 'x'
    x_inverse <- solve(data, ...)
    
    # cache the inverse value of 'x'
    x$setinverse(x_inverse)
    
    # return the inverse of 'x'
    x_inverse
}
