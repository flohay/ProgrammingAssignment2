## The function takes a matrix in input and calculates its inverse
##The inverse is then stocked in cache and get from there if asked again

## This function creates a matrix and a place to cache its inverse once
## it has been calculated

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<-y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse of a special matrix or gets it
## from cache if it has been calculated already

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
