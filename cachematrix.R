## makeCacheMatrix and cacheSolve are used to check and see 
## if the inverse of a matrix has already been computed.
## makeCacheMatrix is composed of 4 functions. Three that are 
## used by cacheSolve and the other (set) is used to change the
## matrix stored in makeCahceMatrix

## cacheSolve checks to see if the inverse has already been 
## stored in m.  If the inverse has been stored in m, it is 
## displayed.  Otherwise, the matrix from makeCacheMatrix is 
## recalled and the inverse is calculated and stored.

## makeCacheMatrix is used to store a list of functions to be 
## executed on the matrix input to makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
     ## Changes the matrix in the main function and flushes m
     set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Recalls the matrix input to makeCacheMatrix
    get <- function() x
    ## Stores the input as the inverse
    setInv <- function(solve) m <<- solve
    ## Gets the inverse of m
    getInv <- function() m
    
    ## The following line stores the 4 functions
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve checks to see if the inverse has been calculated
## and if not, calculates the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    
    ## If m already has the inverse, retrieve m
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## If m is null, get m from makeCahceMatrix
    data <- x$get()
    ## Compute the inverse using solve(m)
    m <- solve(data, ...)
    x$setInv(m)
    m
    }
