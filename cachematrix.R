library("MASS")

## makeCacheMatrix:
##
## creating a cached matrix object. this object store in it's envrionment the result of the matrix inversion
## so it will available when resuse is needed. the object is using
## R lexical scoping rules to store "private" data in makeCacheMatrix environment
##
## methods: 
##  set:        store a new matrix in makeCacheMatrix and reset the cache
##  get:        just return the matrix stored via the set method
##  setinverse: save the inverse result in the cache for later quick reuse
##  getinverse: return the saved inversed matrix from the cache
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    if ( ! is.matrix(x) ) {
        return(NULL)
    }
    
    ## construct makeCacheMatrix object using the lexical scoping rules:
    ##   save y (the input) into the makeCacheMatrix environment NOT in set method env 
    ##   set m to null in the same way.
    ##
    ## to illustrate when set is called the picture is like this:
    ## +----------------------------------------+
    ## |          top level enviroment          |
    ## |                                        |
    ## | myCacheMatrix <- makeCacheMatrix(x)    |
    ## +----------------------------------------+
    ## |      makeCacheMatrix environment       |
    ## | x                                      |
    ## | m                                      |
    ## | set, get, setmatrix, getmatrix         |
    ## + ---------------------------------------+
    ## |          set environment               |
    ## | y                                      |
    ## +----------------------------------------+
    ## note: same goes for all methods
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # just return what was define during makeCacheMatrix creation or as result of set fcunction
    get <- function() x
    
    # save the inversion result. this is the key point here.
    # the operator <<- copy inv into m under the *parent environment*. 
    # NOTE: how m is preserve in makeCacheMatrix's returned object? 
    #   ususally the parent environment will destroy after the function is called and returned it's value/s.
    #   but if any other object *hold* a pointer (a reference) to that return value of makeCacheMatrix it will not distroy by the garbage collector
    #   thats why m and x are kapt alive after makeCacheMatrix is return
    setinverse <- function(inv) { 
        m <<- inv
    }
    
    # return the cached value. 
    getinverse <- function() m
    
    # return the special matrix object. 
    # this special object include the four methods *plus* the m value kapt in makeCacheMatrix environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve
##
## a function take an object of type makeCacheMatrix and return it's inverse
## it try to retrive the the inverse matrix from the cache if fail then calculte the inverse and store it in 
## makeCacheMatrix cache for future reuse
cacheSolve <- function(x, ...) {
    
    ## get the inverse matrix from cache
    m <- x$getinverse()
    
    if( ! is.null(m) ) {
        return(m)
    }
    
    ## not in cache so go head calculate it's inverse.
    data <- x$get()
    if (nrow(data) == ncol(data)) {
        m <- solve(data)
    }
    else {
        m <- ginv(data)
    }
    
    # store in the cache for future reuse
    x$setinverse(m)
    m    
}
