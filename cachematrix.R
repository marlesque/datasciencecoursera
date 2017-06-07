## I have two functions, makeCacheMatrix and cacheSolve, which returns the inverse
## of a matrix. However, it does so by first storing the inverses in a cache and
## checks to see if the inverse has already been computed. If it has already
## been computed, it will extract from the cache the inverse. If not, it will do
## the computation and return the inverse. 

## The function makeCacheMatrix creates a special "matrix" containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set=set, get=get, setInverse=setInverse, getInverse= getInverse)

}
## The following function calculates the inverse of the special matrix created
## with the above function. It first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setInverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- makeCacheMatrix(x)$getInverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- makeCacheMatrix(x)$get()
        m <- solve(data, ...)
        makeCacheMatrix(x)$setInverse(m)
        m
        }
