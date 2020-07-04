#### Caching the Inverse of a Matrix ####
## This pair of functions will cache the inverse of a matrix.

## The function makeCacheMatrix() will create a special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
        
        ## Set 'inv' to NULL as a placeholder for a future value
        inv <- NULL 

        ## Assign the value of 'y' to 'x', Assign the value of NULL to 'inv' (clearing prior values)
        set <- function(y) { 
                x <<- y 
                inv <<- NULL
        } 
        
        ## Retrieve 'x' from the parent environment of makeCacheMatrix()
        get <- function() x 
        
        ## Set the value of the inverse to 'inv'
        setInverse <- function(inverse) inv <<- inverse 
        
        ## Returns the value of 'inv'
        getInverse <- function() inv 
        
        ## Gives the name 'set' to the set() function defined above...
        ## Returns the 'special vector' containing all of the functions just defined
        list(set = set,         
             get = get,
             setInverse = setInverse,
             getInverse = getInverse) 
}

## The function cacheSolve() computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## solve(A): Inverse of A where A is a square matrix.
        getInv <- function() solve
        
        ## Check to see if the inverse of 'x' has already been calculated
        inv <- x$getInv() 
        
        ## If so, gets the inverse from the cache and skips the computation
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) 
        } 
        
        ## Otherwise, calculates the inverse of the 'x'
        data <- x$get()
        inv <- solve(data, ...) 
        x$setInverse(inv) # sets the value of the mean in the cache
        inv
}


## Calling the functions  
## Example Matrix from: https://www.mathsisfun.com/algebra/matrix-inverse.html
matrix.test <- matrix(c(4, 7, 2, 6), 2, 2, byrow = T)
matrix.test
solve(matrix.test)

## Now use the functions
aMatrix <- makeCacheMatrix(matrix.test)
typeof(aMatrix)

aMatrix$get()        # retrieve the value of 'x'
aMatrix$getInv()     # retrieve the value of 'inv', which should be NULL

cacheSolve(aMatrix)  # calculated the inverse of 'x'
aMatrix$getInv()    # retrieve it directly, now that it has been cached
