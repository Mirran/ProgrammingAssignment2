## Two functions are defined here to demonstrate the lexical scoping properties of R
## A second funciton first checks for the existence of a value cached by another function
## (rather than unnecessarily recalculating).
## One function instantiates an invertible matrix and its inverse
## The other function calculates that inverse (and caches it) but only if it doesn't 
## already exist 
## Tests
##  z<-makeCacheMatrix(matrix(c(4,1,3,1),nrow=2,ncol=2)) ## should store matrix
##  z$get()  ## s/b stored matrix
##  z$getinverse() ## s/b NULL
##  cacheSolve(z)  ## s/b calculated inverse -- caches too
##  z$getinverse() ## s/b inverse
##  cacheSolve(z) ## s/b cached inverse

## makeCacheMatrix creates an invertible matrix and can cache its inverse 
##  invertible square matrix assumed
makeCacheMatrix <- function(x=matrix()) {
    
    i <- NULL
    # allow another matrix to be stored and clear the variable holding the inverse matrix 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    ## set names to variables
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve looks for an inverse of the cached by makeCacheMatrix otherwise it 
##  calculates the inverse of the matrix cached by same. 
## invertible square matrix assumed
cacheSolve <- function(x) {
    ## check for and return the cached inverse if available
    i <- x$getinverse()    
    if(!is.null(i)) {
        message("found a cached inverse..")
        return(i)
    }
    ## otherwise calculate the inverse of the matrix in makeCacheMatrix
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}