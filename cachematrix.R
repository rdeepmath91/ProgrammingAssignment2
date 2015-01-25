## Put comments here that give an overall description of what your
## functions do
## First I create makeCacheMatrix function to create a special "matrix" object
## that can cache its inverse. Then I create cacheSolve function to compute the 
## inverse of the "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

## Write a short comment describing this function
## inv is the value of the cached inverse
## makeCacheMatrix returns a list containing the following 4 functions
## $set to set the value of the matrix
## $get to get the value of the matrix
## $setinverse to set the value of the inverse
## $getinverse to get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## first check whether the inverse (inv) has been calculated before
## if yes, then return x$getmean as the inverse
## otherwise, calculate the inverse by solve() function
## cache the value and then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
