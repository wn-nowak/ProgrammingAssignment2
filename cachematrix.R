## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invrt <- NULL
        set <- function(y) {
                x <<- y
                invrt <<- NULL
        }
        get <- function() x
        setInvMtx <- function(inverse) invrt <<- inverse
        getInvMtx <- function() invrt
        list(set = set, 
             get = get,
             setInvMtx = setInvMtx,
             getInvMtx = getInvMtx)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrt <- x$getInvMtx()
        if(!is.null(invrt)) {
                message("getting cached data")
                return(invrt)
        }
        data <- x$get()
        invrt <- solve(data, ...)
        x$setInvMtx(invrt)
        invrt
}
