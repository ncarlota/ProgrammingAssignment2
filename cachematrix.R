
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) inv <<- solve(x)
        getiInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


##Line 13 is defining the intended function

##Since objective is to create a matrix that can store it's content and it's
##inverse, we need 2 variables, x and it's respective inverse.

##`<<-` is used to assign a value to an object in an environment different
##from current, causing a search in such parent environment.
##(x and inv are stored in the enclosing environment of the
## set and get + setInverse and get Inverse functions)

##Continuing, any given variable should have respectively 2 functions:
## Set and Get

##Set would take a given argument and compute it;
##Get returnns the value of a named object.

##In line 15, set takes function(y) and relates it to matrix x
##Resembling the above, inv is also related to NULL


##In line 16, x <<- y creates x, which is attained by the get().

##Line 20 initiates to calculate the inverse, returning it via similar logic





##2. cacheSolve: This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache.

##Computing the inverse of a square matrix can be done with the solve function
##in R. For example, if X is a square invertible matrix, then solve(X) returns
##its inverse.
##For this assignment, assume that the matrix supplied is always invertible.




cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


##assuming x as inverse of the original matrix input to makeCacheMatrix()
##(Line 64) in case the inverse is alreadyh calculated...
##(Line 65 thru 67) ...or else it is given from the cache, skipping to compute it

## Line 70 sets the value of the inverse in the cache via the setinv function

##Line 71 Returns the matrix which is x's inverse       