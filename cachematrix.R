## Below i wrote two functions, makeCacheMatrix() and cacheSolve()
## we create a matrix having 2 'getter' and 2 'setter' functions in it.
## check cache for the inverse and retrieve it if 'not null'
## or else compute the inverse and set it into the cache for use next time.
## Matrix inversion is costly computationally.
## This allows us to play around with the functions within the 
## environment they were defined 


## This function creates a list having 4 functions in it.
## it takes only 1 argument of matrix type.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL           # initialise our vector having the inverse
        set <- function(y) {  # create the set matrix function
                x <<- y
                inv <<- NULL  # set it to null again.
        }
        get <- function() x   # create the get matrix function
        setInverse <- function(inverse) inv <<- inverse # set the inverse to cache
        getInverse <- function() inv # get the inverse from cache
        list(set = set,              # we create our matrix, i.e. the list
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the "matrix" x created by 
## makeCacheMatrix(). checks if the inverse was already been calculated.
## if yes, it retrieves that value from its cache 
## else it calculates the new inverse and sets it into cache then returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse() #we call the getInverse function and put its return into inv
        if (!is.null(inv)) {   ##now we check if there is a cached result in inv
                message("getting cached data")
                return(inv) # if yes, we return the result i.e. the inverse
        }
        mat <- x$get()         # else we get the value of the new matrix
        inv <- solve(mat, ...) # we find the inverse of the new matrix
        x$setInverse(inv)      #we set the new inverse into our special object x
        inv                    # we return the inverse.
}
