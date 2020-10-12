## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## create the details of the inverse matrix
    inv <- NULL

    ## Creation of "Set" which aims to regulate the creation of the matrix
    set <- function( matrix ) {
            m <<- matrix
            inv <<- NULL
    }

    ## With "get" the matrix is obtained, it makes a copy and returns the matrix "m"
    get <- function() m
    
    setInverseMatrix <- function(inverse)
        inv <<- inverse

    ## Its function is to return the details of the inverse matrix, by means of "inv"
    getInverseMatrix <- function() inv

    ## returns everything done in (set, get, setInverseMatrix, getInverseMatrix) this through a list.
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            ## GIM is a summary to call "x$getInverseMatrix()"
    GIM <- x$getInverseMatrix()

 
    ## if the "IF" is valid, GIM returns
    if( !is.null(GIM) ) {
        msgbox("getting cached data")    
        return(GIM)
    }

    ## dataInv is a summary to call "x$get()"
    dataInv <- x$get()

    ## the GIM value is updated with the result of the matrix multiplication
    GIM <- solve(dataInv) %*% dataInv
    
    ## returns
    x$setInverseMatrix(GIM)
    GIM
}
