##
## R Script Name:       cachematrix.R
## Functions Created:   makeCacheMatrix(x)
##                      cacheSolve(x)
##
## Aurthor1:  kluersen
## Aurthor2:  I worked alone.
## Date:      October 16, 2014
## Course:    R Programming (rprog-008)
##
## Script Description: Matrix inversion is usually a costly computation and there may be
##          some benefit to caching the inverse of a matrix rather than computing it
##          repeatedly (e.g.when in a loop). This script enables you to reduce time by 
##          chaching the inverse matrix, allowing for multiple recalls of the inverse 
##          matrix without recalculation.
##
## Warnings: This script works under error free conditions. It does not check for, or 
##          handle, error events.
##


##############################
##
## Function Name:   makeCacheMatrix(x)
##
## Function Description:  makeCacheMatrix creates a special "matrix", which is
##           really a list containing sub-function to:
##
##              1.  set the value of the matrix
##              2.  get the value of the matrix
##              3.  set the value of the inverted matrix
##              4.  get the value of the inverted matrix
##
##          It returns a list where access to the matrix and its cached inverted 
##          matrix are only accessible via the methods contained in the returned list.
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##############################
##
## Function Name:   cacheSolve(x)
##
## Function Description:  cacheSolve calculates the inverse matrix of the special 
##          "matrix"  created with the above function. However, it first checks to see
##          if the inverse matrix has already been calculated. If so, it "get"s the 
##          inverse matrix from the cache and skips the computation. Otherwise, it 
##          calculates the inverse matrix of the cached matrix and sets the inverse 
##          matrix in the cache via the "setinverse" sub-function.
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv

}


#############################
##
##  Examples:
##
##      > source("cachematrix.R") -- Sources this script file
##      > amatrix = makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2)) -- creates our obj.
##      > amatrix$get()           -- Returns original matrix values
##      > amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) -- Modifies existing matrix
##      > cacheSolve(amatrix)     -- Computes, caches, and returns the inverse matrix
##      > amatrix$getinverse()    -- Returns the inverse matrix values
##