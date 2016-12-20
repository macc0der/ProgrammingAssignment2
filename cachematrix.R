## This file has two functions to do matrix operations. The makeCacheMatrix() function creates a sepcial matrix
## object which can store its inverse. The cacheSolve() fucntion calculates the inverse of the special matrix object
## that was created using the makeCacheMatrix function and caches it.

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


## funciton to store the inverse of the special matrix object created using the makeCacheMatrix()
## It first checks to see if the matrix object already has its inverse computed previously,
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
        inv <-solve(x$get())
        x$setInverse(inv)
        inv
}
