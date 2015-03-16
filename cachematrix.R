#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

# very nice explanation here: https://class.coursera.org/rprog-012/forum/thread?thread_id=57

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL
    set <- function(y) {
        x <<- y
        inverseM <<- NULL  # for the first time the matrix is NULL
    }
    get <- function() x
    setinverse <- function(solve) inverseM <<- solve ## updating the global variable
    getinverse <- function() inverseM
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
} 


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

### conclusion:  "inverseM" is a global variable. The first time its value is NULL. But subsequently, when you call the function, the value of inverseM is updated. 
# So in the next function, inverseM's value can be directly obtained from "cache" since its a global variable

## Testing
# p <- matrix(c(1,3,2,4),nrow=2,ncol=2)
# p1 <- matrix(c(1,2,3,4),nrow=2,ncol=2)
# 
# p1c <- makeCacheMatrix(p1)
# pc <- makeCacheMatrix(p)
# 
# cacheSolve(pc)  # first call, so global variable is initially empty. But at the end of the function, its value will be updated
# cacheSolve(pc) # second call, so its value will be retrieved from the cache(global varible environment)
# 
# cacheSolve(pc1) 
# cacheSolve(pc1) 

