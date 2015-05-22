## makeCacheMatrix takes a matrix as an argument and returns a list of 4 functions when called. 
## The 4 functions are
##  1. Set : Set function sets up the data in the matrix and sets the inverted matrix to NULL.  
##  2. Get : Get function simply returns the matrix to the calling environment. 
##  3. Setinv : Sets the value of the inverted matrix to the incoming argument. 
##  4. Getinv :  Returns the Inverted matrix value. 



makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inverse) { 
                inv <<- inverse
        }
        
        
        getinv <- function()    {
                inv
        }

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve is a function that returns the inverse of a square matrix using the solve function when called for the very first time. 
## If cacheSolve is called on the same data again, the fuction forgoes calculations and instead 
## pulls out the pre-existing data from the cache. 

	cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}
