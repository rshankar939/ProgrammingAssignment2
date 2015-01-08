## These function define new object that encapsulates matrix and caches inverse

## makeCacheMatrix function stores the matrix and its inverse value and provides
## 2 accessor methods and 2 modifer methods

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # sets the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # sets the inverse of the matrix
        setinv <- function(s) inv <<- s
        # gets the inverse of the matrix
        getinv <- function() inv
        x1 <- function() inv
        # returns a list of methods
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function is used to compute inverse of an object created by the 
## function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                #getting cached inverse
                return (inv)
        }
        #computing inverse
        data <- x$get()
        inv <- solve(data)
        #setting inverse as a cached value
        x$setinv(inv)
        inv
}
