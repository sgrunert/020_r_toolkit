##############################################################
#Author: S. Grunert
#Create: 19December2014
#Description:
#The following is for Coursera Course: rprog-016: Project 2
#This script is derived from a script forked from:
#https://github.com/rdpeng/ProgrammingAssignment2
##############################################################

##############################################################
## The following functions work together to invert matrices
## while saving off to cache prior inversions to keep from
## reprocessing the same inversion twice. Caching conserves
## CPU processing resources.
##############################################################

##############################################################
## Function makeCacheMatrix() requires a matrix input and
## does the following:
#### 1) Creates a list to hold and track cached matrices with
#### set, get, setinv, and getinv variables, setting the list
#### initially to empty.
#### 2) Creates 3 functions get(), setinv(), and getinv()
#### for use in other functions for managing checking and
#### updating the cache list variables.
##############################################################
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##############################################################
## Function cacheSolve() does the following ONLY for square 
## matrices, such as 2x2,3x3.etc., preprocessed with 
## makeCacheMatrix():
#### 1) Checks to see if the cache is empty using getinv() made
#### in makeCacheMatrix(), and when cache is present, provides
#### the message "getting cached matrix inverse" and then prints
#### the cached matrix.
#### 2) If cache is not present, cacheSolve() uses the function
#### solve() to invert the matrix, and then with functions 
#### get() and setinv() from makeCacheMatrix(), cacheSolve() 
#### pushes the inversion result to the cache list.
#### Note: Function solve() only works for square matrices.
#### See cacheSolve2() below for non-square matrices.
##############################################################
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached matrix inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

#######################################################################
## This alternative cacheSolve2() inverts both square and non-square
## matrices, such as 2x3, 5x9. It operates in the same manner as the
## above cacheSolve(), only the solve() function is replaced with ginv().
## The ginv() function requires the additional MASS package.
#######################################################################
install.packages("MASS")
library(MASS)

cacheSolve2 <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached matrix inverse")
                return(m)
        }
        data <- x$get()
        m <- ginv(data, ...)
        x$setinv(m)
        m
}

##############################################################
##Test scripts.

#squareMatrix1 <- matrix(1:4,nrow=2,ncol=2)
#squareMatrix2 <- matrix(rnorm(9),nrow=3,ncol=3)
#nonsquareMatrix1 <- matrix(rnorm(10),nrow=2,ncol=5)
#squareCache1 <- makeCacheMatrix(squareMatrix1)
#squareCache2 <- makeCacheMatrix(squareMatrix2)
#nonsquareCache1 <- makeCacheMatrix(nonsquareMatrix1)

#cacheSolve(squareCache1)
#cacheSolve2(squareCache1)
#head(squareCache1)

#cacheSolve(squareCache2)
#cacheSolve2(squareCache2)
#head(squareCache2)

#cacheSolve(nonsquareCache1) ##Fails for non-square matrix.
#cacheSolve2(nonsquareCache1)
#head(nonsquareCache1)

