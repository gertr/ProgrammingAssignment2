makeCacheMatrix <- function(v = matrix()) {
    inv <- NULL
    set <- function(w) {
        v <<- w
        inv <<- NULL
    }
    get <- function() v
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function assumes that the matrix is always nondegenerated.
cacheSolve <- function(v, ...) {
    inv <- v$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- v$get()
    inv <- solve(data)
    v$setinverse(inv)
    inv
}
##source("cachematrix.R")
##v<- rbind(c(2,3),c(4,5))
##> y<- makeCacheMatrix(v)
##> y$get()
##     [,1] [,2]
##[1,]    2    3
##[2,]    4    5
##> cacheSolve(y)
##     [,1] [,2]
##[1,] -2.5  1.5
##[2,]  2.0 -1.0
