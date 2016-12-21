## This file has two functions to do matrix operations. The first function creates a sepcial
## object which can store its inverse. The second one calculates the inverse of a matrix
## and caches it.

## function to create a special matrix object that can store its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inv_m)inv<<-inv_m
        getInverse <- function() inv
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## funciton to store the inverse of the special matrix object created using the makeCacheMatrix
## function. It first checks to see if the matrix object already has its inverse computed previously,
## if it finds it then we get the cached value. If the object doesn't have its inverse previously computed, then
## the function computes the inverse of the matrix and stores it on the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv))
        {
                message("returning cached data")
                return(inv)
        }
        inv <-solve(x$get(), ...)
        x$setInverse(inv)
        inv
}
