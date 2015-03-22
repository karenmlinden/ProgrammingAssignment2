## A pair of functions that cache the inverse of a matrix


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
