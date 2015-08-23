## R Programming Course - Assignment 2
##
## The following to functions demonstrate the usage of R's lexical scoping rules
## to create "objects" capable of holding state in between calls.
##
## In reality the "state" is held by the environment associated with the functions
## created by makeCacheMatrix
##

## makeCacheMatrix
##
## Description: When this function is called an environment containing two variables and
##              four functions is created. The variables are the matrix object (x) passed
##              as argument and its inverse (inv). The four functions defined by makeCacheMatrix
##              will keep a reference to this environment and will operate over these two variables
##              In more technical terms these four functions are closures that will keep
##              a reference to the environment where they were defined, including its variables
##              and their values.
##
## Args       : x - the matrix object that will be stored in the environment
## Return     : a list of functions (closures) that keep a reference to their definition environment,
##              its variables and their values
##

makeCacheMatrix <- function(x = matrix()) {
    
    ## This variable will hold the inverse matrix
    inv <- NULL
    
    ## Change the matrix value and reset the cache variable of its inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Simply return the matrix object
    get <- function() {x}
    
    # Cache the value of the matrix inverse
    setinverse <- function(inverse) {inv <<- inverse}
    
    # Simply return the matrix's inverse
    getinverse <- function() {inv}
    
    # return a list of functions (closures) that reference the environment
    # where the original matrix (x) and its inverse (inv) are stored
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
##
## Description: This function calculates the inverse of the "special matrix" object
##              passed as argument. In reality x is not a matrix at all but a list of
##              functions (closures) pointing to the environment where the actual matrix
##              object is stored. It first tries to retrieve the inverse value from
##              the environment and if one exists, job done, it simply returns that.
##              Otherwise it will calculate the inverse matrix and store it in the
##              environment before it is returned. Future calls will find the stored
##              value and return immediatly without calling solve().
##
## Args       : x - the "special matrix" object for which we want to calculate an inverse
## Return     : the inverse of x
##
cacheSolve <- function(x) {
    
    ## First lets check if the inverse value has been calculated already
    inv <- x$getinverse()
    
    ## If not null then it has and lets just return it. Job done!
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    ## If the value returned was null then let's...
    inv <- solve(x$get())    ## calculate it...
    x$setinverse(inv)        ## store it...
    inv                      ## return it!
}
