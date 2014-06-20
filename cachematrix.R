# 2014-06-20 | Nick Shopov | Contacts: www.nickshopov.com
# R Programming Assignment 2

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) {        # accepts matrix as an input argument
        mInv <- NULL                             # initialize the variable to accommodate the inverse                                       
        set <- function(mGlobalMatrix)
        {
                x <<- mGlobalMatrix              # sets a matrix to the global environment 
                mInv <<- NULL                    # and nullify its Inverse
        }                              
        get<-function() 
        {
                x                                # reads a matrix that is passed as a parameter
        }                                     
        setInv<-function(mGlobalInverted)
        {
                mInv <<- mGlobalInverted         # sets the inverse of a matrix to the global environment
        }
        getInv<-function()
        {
                mInv                             # gets the inverse of a matrix from the global environment
        }
        # the output of the function is a list of references to these 4 default functions
        list (set=set, get=get, setInv=setInv, getInv=getInv) 
}

# This function creates a special "matrix" object that can cache its inverse.This function computes the inverse 
# of the special "matrix" returned by makeCacheMatrix(). If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache. For this assignment, assume 
# that the matrix supplied is always invertible.

cacheSolve <- function (x, ...) {       # "x" is a list with 4 elements as returned from makeCacheMatrix()
        
        mInv <- x$getInv()              # get the inverse value from the global environment
        if (!is.null(mInv))             # check if an inverse has already been calculated for this matrix
        {
                message("This matrix comes from cache:")           # if positive, then print out an info message
                return(mInv)                                       # and return the inverse matrix from cache
        }
        data <- x$get()                 # otherwise call the get() method of "x" to read the matrix into memory              
        mInv <- solve(data, ...)        # calculate the inverse. It is convertible by default, no check needed.
        x$setInv(mInv)                  # set the result to the global environment
        mInv							# return the inverse
}

# Test:
# obj<-makeCacheMatrix(matrix(1:4,2,2))		# initialize a new matrix object - OK
# cacheSolve(obj)							# calculate the inverse - OK
# cacheSolve(obj)							# attempt to calculate again, then cache is used - OK			