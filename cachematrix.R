## A pair of functions that cache the inverse of a matrix

## Example Matrix
## matrix A=
##     [,1]  [,2]
##[1,]  1.0  2.0
##[2,]  2.0  4.0

## work
## A-1 = - 0.5
##     [,1]  [,2]
##[1,]  4.0 -2.0
##[2,] -3.0  1.0

## inverse A= 
##     [,1]  [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5

## Examples Run Code
## inv <- matrix(data = c(1,3,2,4), nrow = 2, ncol = 2)
## inv2 <- makeCacheMatrix(inv)
## cacheSolve(inv2)

## makeCacheMatrix creates a matrix that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL  ## sets the value of m to NULL
        set<-function(y){ ## set the value of the matrix
                x<<-y     ## caches the inputted matrix 
                m<<-NULL  ## sets the value of m to NULL
        }
        get<-function() x
        setinverse<-function(solve) m<<- solve
        getinverse<-function() m
        list(set=set, get=get,  ## creates a list the four functions
             setinverse=setinverse,
             getinverse=getinverse) 
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Needs to compare matrix 
        m<-x$getinverse()   ## if an inverse has already been calculated this gets it
        if(!is.null(m)){   ## check that matrix hasn't changed
                message("getting cached data")
                return(m)
        }
        matrix <- x$get() 
        m<-solve(matrix, ...)
        x$setinverse(m)   ## run the setinverse function 
        m ## return the inverse
}
