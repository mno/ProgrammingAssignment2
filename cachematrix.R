## This file contains the necessary functions to construct 
## a CacheMatrix object and calculate its inverse. The advantage
## of this special matrix is that it caches its inverse the first time
## that it is calculated. This eliminates the need to recalculate the
## inverse of the same matrix which is often an expensive computation,
## especially for large matrices. An example of how to use the
## function is included at the end of this file.

## This function is used to construct a CacheMatrix object from a
## given ordinary matrix.
##
## Arguments:
##     x : This is an ordinary matrix that can be created using
##     the matrix(.) function in R. This an optima argument and if
##     it is not provided the object will be initialized with an
##     empty matrix.
##
## Return type:
##     this function returns a list that contains a reference to the
##     following functions:
##         1) set : this function accepts one argument and can be used
##            to set the variable 'x' which is an ordinary R-type
##            matrix.
##         2) get : this function has no argments and returns the
##            variable 'x' which is defined in the argument list of
##            makeCacheMatrix.
##         3) setinverse : this function accepts one argument and
##            sets the free variable 'inverse' that is used to cache
##            the inverse of variable 'x'
##         4) getinverse : this function do not have any arguemtn
##            and returns the value of the free variable 'inverse'
##            in makeCachedMatrix.

makeCacheMatrix <- function(x = matrix()) {
    
    # This variable is used to cache the inverse of 'x'. It is
    # initialized to NULL to signal the cacheSolve to calculate it
    # for the first time.
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inv) inverse <<- inv
    
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function is used to calculate the inverse of a matrix which
## is created by makeCachedMatrix. This function calculates the inverse
## of the input matrix the first time that it is passed and stores the
## inverse back into the special CachedMatrix for future references. If
## the input matrix has a cached inverse then this function return
## the cached version and prints a notification to the console.
##
## Arguments:
##     x : this is a special matrix which is created by
##         makeCachedMatrix function.
##
## Return value:

cacheSolve <- function(x, ...) {
    
    # Here the value of the free variable 'inverse' which is defined
    # in makeCachedMatrix is retrieved. If this variable is not NULL,
    # it shows that the inverse was calculated previously which is
    # returned by the function.
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # If 'inv' is NULL it means that the inverse has not been
    # calculated before. Therefore, the get function is used to
    # retrieve the R-type matrix which is stored in data. Then, the
    # solve function of R is used to calculate its inverse and finally
    # the inverse is store back into 'x' using the function setinverse.
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# EXAMPLES
# -------- 
#
# > m1 = matrix(runif(4), 2, 2)         # creating an ordinary 2x2 matrix.
# > m2 = matrix(runif(4), 2, 2)
#  
# > print(m1)
#           [,1]      [,2]
# [1,] 0.4119827 0.5334610
# [2,] 0.5450361 0.5363164
#
# > print(m2)
#            [,1]      [,2]
# [1,] 0.04715963 0.7200101
# [2,] 0.33988370 0.8340433
#
# Initialization Type 1:
# > CachedMat1 <- makeCacheMatrix(m1)   # constructing a CachedMatrix with m1.
#
# Initialization Type 2:
# > CachedMat2 <- makeCacheMatrix()     # constructing an empty CachedMatrix.
# > CachedMat2$set(m2)                  # initializing the internal matrix.
#
# > print(cacheSolve(CachedMat1))       # first calculation of inverse.
#           [,1]      [,2]
# [1,] -7.683351  7.642443
# [2,]  7.808271 -5.902127
#
# > print(cacheSolve(CachedMat1))       # returning the cached inverse.
# 
# getting cached data
#           [,1]      [,2]
# [1,] -7.683351  7.642443
# [2,]  7.808271 -5.902127


