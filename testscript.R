m1 = matrix(runif(4), 2, 2)         # creating an ordinary 2x2 matrix.
m2 = matrix(runif(4), 2, 2)

print(m1)
print(m2)
CachedMat1 <- makeCacheMatrix(m1)   # constructing a CachedMatrix with m1.
CachedMat2 <- makeCacheMatrix()     # constructing an empty CachedMatrix.
CachedMat2$set(m2)                  # initializing the internal matrix.
print(cacheSolve(CachedMat1))       # first calculation of inverse.
print(cacheSolve(CachedMat1))       # returning the cached inverse.
print(cacheSolve(CachedMat2))       # first calculation of inverse.
print(cacheSolve(CachedMat2))       # returning the cached inverse.

