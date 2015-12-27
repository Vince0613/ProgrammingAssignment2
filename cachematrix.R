## Creates a special version of a matrix, computes and caches the
## results for later use
    
## Creates a special version of a matrix that can have its
## inverse cached
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This returns a cached version of the solve() matrix if it exists
## otherwise it computes solve() for the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
