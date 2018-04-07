## Compute the inverse of an invertable Matrix and keep the result as 
## cache. If the inverse needs to be recalculated later then use the 
## cached data instead of calculating it again. As calculating inverse of
## a matrix is computationally expensive, it will save computaion time.



## Creates a special Matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse 
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Computes the inverse of the special matrix returned by makeCacheMatrix 
## If the inverse has already been calculated and the matrix has not changed,
## then the cachesolve should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("Getting Cached Data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
