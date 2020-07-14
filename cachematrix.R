## This pair of function(makeCacheMatrix(),cacheSolve) can calculte inverse matrix of a invertible matrix.
## 1st function can catch the calculted matrix so that recalculation can skip.It has four different
## function to do operation.
## 2nd matrix can calculate inverse matrix and assing it to diffewrent environment.




## makeCacheMatrix() function initialize a matrix object and store inverse matrix that we get from 
## cacheSolve() which indicates a different environment. That's why we use <<- operator
## because it use to assing object from one environment to another. This function has four
## methods for getting and setting data. Method get() and getmean() use lexical scoping. 
## It returns a list of function.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(inverse){
        m <<- inverse
    }
    getInverse <- function(){
        m
    }
    list(set = set, get = get, setInverse = setInverse,  getInverse=getInverse)

}


## cacheSolve() function use makeCacheMatrix object as argument to calculate inverse matrix.
##  It is designed for calculating inverse using different method of makeCacheMatrix().
## It returns inverse matrix.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)){
        message("Getting Catched Data")
        return(m)
        
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}


m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
mat <- makeCacheMatrix(m1)
cacheSolve(mat)
