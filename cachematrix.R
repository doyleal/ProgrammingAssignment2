##makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##cacheSolve takes an output from makeCacheMatrix.  If the output is stored, it saves

##makeCacheMatrix
## sets the value of the matrix
## gets the value of the matrix
## sets the value of the variable that will reflect computation of the inverse of the matrix
## gets the value of the variable that will reflect computation of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##Gets the value of the matrix
        get <- function() {
                x
        }   
        ## sets the value of variable that will represent the computation of the 
        ##inverse of a square matrix to "m"
        setinv <- function(inv) {
                m <<- inv
        }
        ## gets the value of variable that will represent the computation of 
        ## the inverse of a square matrix
        getinv <- function() {
                m
        }
        ## outputs a list of the functions and names them
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## cacheSolve returns a matrix that is the inverse of x.  first it checks to see if the inverse
## has already been calcuated. if it has, it takes it from cache.
## if it has not, it computes the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setinv(m)
        m
}
