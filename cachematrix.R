# Create a special matrix object that can cache its inverse.
# It includes four internal functions: set, get, setInverse and getInverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() { x }
    setInverse <- function(inverse) { m <<- inverse } # m is not in current environment,
                                                      # <<- operator is needed.
    getInverse <- function() { m }
    
    # List of internal methods
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


# Compute the inverse of the special matrix returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not
# changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (is.null(m)) { # cache miss
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
    }
    else { # cache hit
        message("getting cached data")
        return(m)
    }
}

