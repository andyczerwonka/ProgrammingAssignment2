
## Programming Assignment starts here
# makeCacheMatrix creates a special "matrix", which is really a list containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverted matrix
# 4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

# The following function calculates the inverse of the matrix 
# created with the above function. However, it first checks to see if 
# the inverse has already been calculated. If so, it gets the inverse from 
# the cache and skips the computation. Otherwise, it calculates the 
# inverse of the matrix and sets the value of the inverse in the cache via 
# the setInverse function.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    square <- x$get()
    inverse <- solve(square)
    x$setInverse(inverse)
    inverse
}
