

## Creates a matrix that can cache inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse 
    i <- NULL

    ## Set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## List of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated
## then the "cachesolve" should retrieve the inverse
cacheSolve <- function(x, ...) {

    ## Return the inverse of 'x'
    m <- x$getInverse()

    ## return the inverse if already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get matrix from object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}