## This code calculates the inverse of an input matrix (x) and stores the 
## matrix result in the cache (m). The stored cache result is returned after the 
## calculation has been performed once, as long as the input matrix does not 
## change. The stored cache result is cleared and re-calculated if the input
## matrix changes.

## This function creates an empty object in the parent environment (m) and adds
## the input matrix (x) to the cache via y. m will be the inverse matrix once
## calculated.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL                          # creates m an empty object in the parent environment
        
        set <- function(y){              # The set function is adding the input matrix (x) to the cache 
                x<<-y                    # y is the cached (x) Matrix
                m<<-NULL                 # m, the inv matrix, is cleared from the parent environment 
        }
        
        get <- function() x              # This is the "getter" of the x elements
        setinv <- function(solve)        # This is the "setter" for m - ie it calculates the inverse
                m <<- solve
        getinv <- function() m           
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## This function returns the matrix that is the inverse of 'x'
## 


cacheSolve <- function(x, ...) {
        
        m <- x$getinv()  # Get the cache inverse of the Matix x, if available
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()        # If there is no cached inverse matrix then 
        m <- solve(data, ...)  # it is calculated, stored in cache and the
        x$setinv(m)
        m                      # inverse matrix is returned to the console
}
