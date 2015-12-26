## The following two functions will cache and compute the inverse of a matrix.
## The first function makeCacheMatrix will compute and cache the inverse matrix 
## and the second function cacheSolve will check if the inverse is cached in the 
## first function and get it otherwise it will compute the inverse.

## The function makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) inv<<- solve
    getmatrix<-function() inv
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## The function cacheSolve first checks if the inverse has already been computed 
## in makeCacheMatrix, if the inverse has been cached it gets that and skips the 
## computation. If not it will compute the inverse and cache it through 
## setinverse function.## The function cacheSolve first checks if the inverse has already been computed 
## in makeCacheMatrix, if the inverse has been cached it gets that and skips the 
## computation. If not it will compute the inverse and cache it.

cacheSolve <- function(x, ...) {
    inv<-x$getmatrix()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    matrix<-x$get()
    inv<-solve(matrix, ...)
    x$setmatrix(inv)
    inv
}
