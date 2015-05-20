## Matrix inversion can be computationally costly.  The following functions
## allow for caching of the inverse of a matrix and using the cached inverse
## rather than repeated computation of the inverse of the same matrix.
## If the inverse has not alredy been cached, then it is computed.

## The first function, makeCacheMatrix creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## assigns a null matrix to m
  set <- function(y){
    x <<- y
    ## assigns y to x in the parent environment
    m <<- NULL
    ## assigns a value of NULL to m in the parent environment
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  ## scopes the value of m in the parent environment
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second fuction, cacheSolve computes the inverse of 
## the special "matrix" created by above function.  However, 
## it first checks to see if the inverse has already been
## calculated, in which case, it skips the computation and
## gets the invesrse from the cache.  Otherwise, it calculates
## the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  ## gets cached inverse matrix and assigns it to m
  if(!is.null(m)) {
    ## check to see if the m is not a null matrix
    message("getting cached inverse")
    ## if m is not null, then print the above message
    return(m)
    ## and return m as output
  }
  mm <- x$get()
  ## otherwise, assign the input matrix to mm
  m <- solve(mm, ...)
  ## compute the inverse for mm
  x$setinv(m)
  ## cache this inverse matrix
  m
  ## return this inverse matrix
}