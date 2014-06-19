## Here we have two functions named 'makeCacheMatrix' and 'cacheSolve'.
##
## 'makeCacheMatrix' Will create a structure with a given Matrix, and this
## ----------------  structure will store its inverse, and that will avoid
##                   recalculating it many times.
##
## 'cacheSolve' Will check if there's an inverse matrix pre-calculated in a
## ------------ 'CacheMatrix' object. If there's no inverse, it will calculate
##              it and store it in the object structure.


##
## Note to peer partners: Sorry if my English is not good enough. I am actually
## ---------------------- learning to speak english. So, be patient!. Thanks.


## -------------------------------------------------------------------------- ##


## Purpose: Given a matrix 'x', the function will return a list with four
##          parameter functions. This "structure" will let us store the original
##          matrix, and with the 'cacheSolve' function we will calculate and 
##          store only once the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Initializing the inverse matrix with NULL
        x_inv <- NULL
        
        ## Declaring the 'set' function:
        set <- function(y){
                ## Setting 'x' in the parent environment:
                x <<- y
                
                ##    Initializing the inverse matrix (parent environment) with 
                ## NULL:
                x_inv <<- NULL
        }
        
        
        ## Declaring the 'get' function:
        get <- function(){
                ## Returning the "data" stored in the structure:
                x
        }
        
        
        ## Declaring the 'setInv' function:
        setInv <- function(inverseMatrix){
                ## Setting the inverse matrix in the parent environment:
                x_inv <<- inverseMatrix
        }
        
        ## Declaring the 'getInv' function:
        getInv <- function(){
                ## Returning the inverse function stored in the structure:
                x_inv
        }
        
        ##     Returning a list with the parameters functions 'set', 'get', 
        ## 'setInv', 'getInv':
        list( set = set, get = get, setInv = setInv, getInv = getInv )
}


## Purpuse: Given an object 'x' coming from 'makeCacheMatrix' function, the idea
##          is to provide the inverse matrix stored in that object.
##          It takes the "inverse" matrix stored in 'x'. If that variable is
##          still set in NULL, then the function will use 'solve()' to obtain
##          the inverse matrix, stores inside the structure of 'x' and returns
##          it as output.

cacheSolve <- function(x, ...) {
        ## Getting the object stored in 'x_inv' inside 'x':
        x_inv <- x$getInv()
        
        ## Checking if this object is NULL or not:
        if( ! is.null(x_inv) ){
                ## Indicating that the inverse matrix has been calculated, and
                ## will only be printed:
                message("Getting cached data:")
                return(x_inv)
                
        } else {
                ## If it is NULL, let's calculate and store it:
                
                ## Getting the matrix data inside 'x':
                data <- x$get()
                
                ## Calculating the inverse matrix and storing it in 'data_inv':
                data_inv <- solve(data)
                
                ## Storing it inside 'x' structure:
                x$setInv( data_inv )
                
                ## Returning the inverse matrix:
                return(data_inv)
                
        }
}
