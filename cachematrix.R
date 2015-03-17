## this script consists of 2 functions makeCacheMatrix() and cacheSolve()
## These are used to find the inverse of matrices and to illustrate the concept of lexical scoping in R language.


## makeCacheMatrix()
## This function creates a special "matrix" object that can cache its inverse.
## It returns a list which has 4 functions

makeCacheMatrix<-function(x=matrix()){
        inv<-NULL
  
  ## assigns the value of the matrix in the global environment using the '<<-' operator
        setMatrix<-function(y){ 
                x<<-y           
                inv<<-NULL
        }
  
  ## gives the value of x. Though the x is inside the body of setMatrix function, since '<<-' operator
  ## is used, x is available in the environment of getMatrix and does not die after the setMatrix is executed.
        getMatrix <- function() x     
  ## the setInverse function when called from the cacheSolve function passes the calculated inverse matrix and this gets 
  ## assigned to the 'inv' variable in the global environment using the '<<-' operator
        setinverse <- function(inverse) inv <<- inverse  
  ## getInverse() - gives the value of 'inv' as it can be accessed in its local environmemt
        getinverse <- function() inv
  ## list to be returned is populated with the functions
        list(setMatrix = setMatrix, getMatrix = getMatrix,
                setinverse = setinverse,
                getinverse = getinverse)
}

##cacheSolve()
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve<-function(x,...){## x is the matrix returned by makeCacheMatrix
## getinverse function of the special matrix is called and the cahed value captured in 'inv'
        inv <- x$getinverse() 
## checking if inverse is available or NULL
        if(!is.null(inv)) {      
                message("getting cached data")
                 return(inv)
         }
##getMatrix function of the special matrix is called and value captured in 'data'
        data <- x$getMatrix() 
## calculating inverse of the matrix got from cache
        inv <- solve(data, ...)
##storing the value in cache
        x$setinverse(inv)   
        inv
}
