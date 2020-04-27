#A pair of functions that cache the inverse of a matrix
#Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinv <- function(invererse) inver <<- invererse
        getinv <- function() inver
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

#Compute the inverse of the special matrix returned by "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix #has not changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        
        inver <- x$getinv()
        if (!is.null(inver)) {
                message("Inverse cached data")
                return(inver)
        }
        mat <- x$get()
        inver <- solve(mat, ...)
        x$setinv(inver)
        inver
}