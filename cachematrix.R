## The makeCacheMatrix function creates a special "matrix"-object 
## x which is capable of cacheing its inverse. The casheSolve
## function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from cashe.


## Creates a matrix x and defines functions to return this matrix
## (get), to set the content of x (set), to cache the inverse of x
## (setinverse) and to return the inverse of x (getinverse)


makeCacheMatrix <- function(x = matrix()) {
       inverse <- NULL
       set <- function(y){
               x <<-y
               inverse <<- NULL
       }
        get <- function() x
       setinverse <- function(solve) inverse <<-solve
       getinverse <- function() inverse
       list(set=set, get=get,
            setinverse=setinverse,
            getinverse=getinverse)
}


## Computes,caches and returns the matrix inverse. If an 
## inverse has already been computed it returns the cashed
## inverse.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
        

}

