
## Since matrix inversion is time consuming calculation,
## this code performs it only once to matrix x and 
## then caches the result to be reused next time inverse matrix is neede
## eliminating need to recalculate the inverse matrix more than once.
## The solution consists of two functions:
## makeCacheMatrix and cacheSolve

## makeCacheMatrix function creates an object which
## stores following variables:
## - x that is the matrix given as argument to the function
## - inverseMatrix that is the inverse of x. If iverse in not yet calculated, 
##   inverseMatrix is matrix of NAs
## The function returns a list of 4 methods
## - set for setting x
## - get for getting x
## - setInverseMatrix for setting inverse matrix
## - getInverseMatrix for getting inverse matrix
## 

## This package is required for testing whether the matrix is invertible
install.packages("matrixcalc")
library(matrixcalc)

makeCacheMatrix <- function(x = matrix()) {
        ## test whether matrix x is invertible
        if (is.singular.matrix(x) == TRUE) {
                message("matrix is not invertible")
                return()}
        inverseMatrix <- matrix()
        set <- function(y) {
                x <<- y
                inverseMatrix <<- matrix(data=NA, nrow = y.nrow, ncol = y.ncol)
        }
        get <- function() x
        setInverseMatrix <- function(solve) inverseMatrix <<- solve
        getInverseMatrix <- function() inverseMatrix
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## cacheSolve takes as input the object created by makeCacheMatrix
## It will test whether the object already has cached inverse matrix
## In case cached inverse matrix in  not yet calculated, it will calculate inverse matrix
## and store it to the object x.
## In case there is cached inverse matrix already calculated, it will use that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverseMatrix()
        if(!is.na(inverseMatrix[1,1])) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setInverseMatrix(inverseMatrix)
        inverseMatrix
}
