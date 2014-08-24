## Note: Comments below borrow some terms from Object Oriented Programming.
## For example, Object - a complex user-defined data type that contains
##                       its own data and functions that manipulate its data.
##
##              method - a function that uses the objects data as well as
##                       passed parameters to effect changes to the object
##                       data or return an object or value.
##                       
##
## makeCacheMatrix - initialize an "object", call it CacheMatrix object.
##                   The function requires a matrix as input parameter.
##                   It contains methods to:
##                         set
##                         get
##                         setinverse 
##                         getinverse.
##                   It also keeps a copy of the matrix
##                   and initializes a cache of its inverse matrix.
##
##
## cacheSolve - calculates the inverse of a matrix. 
##              It requires a CachMatrix object (see above) as input.
##              It returns the cached inverse if available.
##
##
## makeCacheMatrix (requires matrix as input parameter)
##
##                 initializes a cache to store the matrix inverse
##                 creates methods to set and get the matrix
##                 creates methods to set and get the inverse of the matrix
##
## -----------------
## Usage and Testing:
## -----------------
##
## m1 <- rbind(c(9,7,0),c(3, -21, 8), c(-3, 14, 8))
## m1_cached <- makeCacheMatrix(m1)
## cacheSolve(m1_cached)
## 
## The results of cacheSolve(m1_cached) should match solve(m1).
## And calling cacheSolve(m1_cached) again should provide 
## the same result but with the function indicating that
## it is pulling the result from the cached inverse as below:
##
##
## > cacheSolve(m1_cached)
## getting cached inverse
## [,1]        [,2]        [,3]
## [1,] 0.098039216  0.01960784 -0.01960784
## [2,] 0.016806723 -0.02521008  0.02521008
## [3,] 0.007352941  0.05147059  0.07352941
## >
##
## Also test using random uniform generator
## m2 <- cbind(runif(4,12,30), runif(4,0,50),runif(4,0,6),runif(4,0,15))
## m2_cached <- makeCacheMatrix(m2)
## cacheSolve(m2_cached)
## ...
##
## -- code follows --
##

makeCacheMatrix <- function(m = matrix()) {
    ## this is the inverse cache for this object
    inv <- NULL
    
    ## create methods .. set, get, setinverse & getinverse
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    ## return this object as list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve (requires CacheMatrix object as input parameter)
##
##              When calculating the inverse, it will check first if 
##              the object's cache already contains the inverse.
##              It returns the calculated inverse if found. Otherwise,
##              it will solve for the inverse matrix and stores the
##              result before returning the inverse.

cacheSolve <- function(x, ...) {
    ## get the cached inverse
    inv <- x$getinverse()
    
    ## has the inverse been calculated?
    if(!is.null(inv)) {
        ## .. yep! .. return it from cache
        message("getting cached inverse")
        return(inv)
    }
    
    ## .. nope .. calculate it from the object's matrix
    data <- x$get()
    inv <- solve(data, ...)
    
    ## remember the inverse by putting it into the cache
    x$setinverse(inv)

    ## returning ..
    inv
}
