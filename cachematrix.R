## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: This function creates "matrix" object, can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" 
#             returned by makeCacheMatrix above. If the inverse has already been 
#             calculated (and the matrix has not changed), then cacheSolve should 
#             retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function 
#                in R. For example, if X is a square invertible matrix, then 
#                solve(X) returns its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #Creates matrix object that will store inverse
        inv <- NULL          #starts as NULL, holds inverse
        set <- function(y) {
                x <<- y      #x is the value of the matrix being set
                inv <<- NULL #this clears the value of inv when we set x
        }
        get <- function() x  #get functions return stuff
        setinverse <- function(inverse) inv <<- inverse  #assigns the value in the parent environment
        getinverse <- function() inv                     #gets assigned value of inv
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)  
        #assigns function to a list(), gives a name, returns to parent environment.
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        #Checks for an inverse in the cache
        #Creates inverse matrix if there is none
        inv <- x$getinverse()  #this is checking whether there's an inv
        if(!is.null(inv)) {    #if there is one, it just prints that
                message("getting cached data")
                return(inv)
        }
        data <- x$get() #gets the data in x
        inv <- solve(data, ...) #the solve function literally solves the matrix without the b argument
        x$setinverse(inv)
        inv
}
