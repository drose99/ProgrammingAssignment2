#Create environment for storage of the makeCacheMatrix function for use by cacheinvmatrix
e <- new.env()

#creates a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        #Initialize the inverse matrix cache as empty/null
                e$invmatrix <- NULL
                
        #Define a function to set the value of the matrix
                set <- function(y) {
                        x <<- y
                        e$invmatrix <<- NULL
                }
        #Define a function to retrieve the matrix x
                e$get <- function() x
        
        #Define a function to set the value of the inverse matrix of x
                e$setinvmatrix <- function(solve) e$invmatrix <<- solve
        
        #Define a function to retrieve the inverse matrix of x
                e$getinvmatrix <- function() e$invmatrix
                
        #List out the four defined functions        
                list(set = set, get = get,setinvmatrix = e$setinvmatrix,
                        getinvmatrix = e$getinvmatrix)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cachesolve <- function(x, ...) {
        #Evaluate if the invmatrix exists in cache, return cached data if exists
                invmatrix <- e$getinvmatrix()
                if(!is.null(e$invmatrix)) {
                        message("getting cached data")
                        return(e$invmatrix)
                }
        #calculate invmatrix if not existing in cache
                data <- e$get()
                invmatrix <- solve(data)
                e$setinvmatrix(invmatrix)
                invmatrix
}
