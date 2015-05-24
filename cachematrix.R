## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(A = matrix()) {
    # initialize the inverse matrix
    A_inv <- matrix(nrow = nrow(A), ncol = ncol(A))
    
    # set method 
    set <- function(B) {
      A <<- B
      A_inv <<- matrix(nrow = nrow(B), ncol = ncol(B))
    }
    
    # get methods
    get <- function() A
    
    # set inverse of A
    set_inv <- function(inverse) A_inv <<- inverse
    
    # get inverse of A
    get_inv <- function() A_inv
    
    # list function methods
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(A, ...) {
    ## try geting the inverse from cache
    A_inv <- A$get_inv()
    
    ## if inverse matrix is not null
    if(!all(is.na(A_inv))) {
      message("getting cached data")
      return(A_inv)
    }
    
    ## if inverse is null, get entries of A
    message("computing inverse")
    entries <- A$get()
    
    ## find the inverse
    A_inv <- solve(entries)
    
    ## set the inverse
    A$set_inv(A_inv)
    A_inv  

}
