## This program aims to cache the inverse of a matrix.

## return a list containing the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(matInv) inv <<- matInv
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## check whether the inverse of the matrix is calculated
## if yes, use the results; if not, calculate the inverse 
## and store it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)){
        message("getting cached inverse of the matrix")
        return(inv)
    }
    mat <- x$set()
    inv <- solve(mat)
    x$setInv(inv)
    inv
}
