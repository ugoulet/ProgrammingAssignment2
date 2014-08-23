## makeCacheMatrix and cacheSolve are a pair of functions
## that will compute the inverse of a matrix and 
## cache it's value

## makeCacheMatrix will create a matrix and include a list of objects that will
## later be called upon by cacheSolve to set the value of the matrix, get the
## value of the matrix, set the value of the inverse, and get the value of the
## inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve will calculate/retrieve the value of the inverse; the first time
## it runs, cacheSolve will compute the inverse of the matrix and will store it;
## if the inverse has alerady been computed, cacheSolve will skip the computation
## and retrive the value of the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
