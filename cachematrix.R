## Written by "jbarnett-gt" for Coursera R Programming course - Jan. 16, 2017.
## The following functions create a matrix object, are able to compute
## its inverse, and if it already exists, retrieves it from cache.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(solut) minv <<- solut
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes inverse of the matrix from above or, 
## if it had been already computed and the matrix remains unchanged, 
## retrieves the inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()
    if(!is.null(minv)) {
        message("getting cached inverse")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    minv
}