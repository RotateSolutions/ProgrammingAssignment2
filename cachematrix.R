## The function makeCacheMatrix is designed to store a matrix and its inverse.
## That way, if the inverse of the matrix is needed, the cached inverse can be retrieved 
## without expending the resources to calculate again the inverse of the original matrix itself. 
## More precisely, what is stored is a vector of 4 functions that 
        ## (1) store the matrix 
        ## (2) retrieve the matrix
        ## (3) store (a matrix presumed to be) the inverse
        ## (4) retrieve the inverse
## respectively:
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve acts upon the 4-vector described above to obtain the inverse matrix.
## If the inverse matrix has already been calculated and stored, the stored value is returned
## without the necessity of calculating it over again. If the inverse has not been stored, 
## the inverse is calculated from the original matrix, stored for future retrieval, and returned:
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        original <- x$get()
        i <- solve(original)
        x$setinverse(i)
        i
}
