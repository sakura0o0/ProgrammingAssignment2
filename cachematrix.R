## Computing the inverse of a square matrix by 
##caching the inverse of a matrix 
##rather than compute it repeatedly. 

##This function creates a special "matrix" object
##that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<-NULL
    }
    get <- function() x
    setSolve  <- function(InverseM) m <<- InverseM
    getSolve <- function() m
    list(set=set,get=get,
         setSolve = setSolve,
         getSolve = getSolve)
    

}


## computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been
##calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse
##from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getSolve()
    if(!is.null(im)){
        message("getting cached data")
        return (im)
    }
    data <- x$get()
    im <- solve(data,...)
    x$setSolve(im)
    im
}
