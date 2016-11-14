

# In this example we introduce the <<- operator which can be used to assign a value
# to an object in an environment that is different from the current environment. 
# Below are two functions that are used to create a special object that stores a 
# numeric vector and cache's its mean.

# The first function, makeVector creates a special "vector", which is really a list containing a function to

# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(
        set = set,
        get = get,
        setmean = setmean,
        getmean = getmean
    )
}

# The following function calculates the mean of the special "vector" 
# created with the above function. However, it first checks to see if 
# the mean has already been calculated. If so, it gets the mean from 
# the cache and skips the computation. Otherwise, it calculates the 
# mean of the data and sets the value of the mean in the cache via 
# the setmean function.

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}



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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
}
