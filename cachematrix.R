## Caching the Inverse of a Matrix
##
## This function creates a special "matrix" object that is able to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
## set the value of the vector
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
## get the value of the vector
        get <- function() x
## set the value of the inverse
        setinverse <- function(inverse) inv <<- inverse
## get the value of the inverse        
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
## If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve  should retrieve the inverse from the cache.
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
## If the inverse hasn´t already been calculated, then the function computes the inverse of special matrix.
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        return(inv)
}
