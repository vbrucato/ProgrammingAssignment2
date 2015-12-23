## Those functions permit the creation of a matrix and to calculate it's inverse.
## It assumes that the matrix is always invertible and thus square.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {                          ## Modifies the vector in the makeCacheMatrix environnement.
    x <<- y                                     ## 'Super' assignement to 'x' and 'inv' so they work in all
    inv <<- NULL                                ## the makeCacheMatrix environnement and not only in 'set'.
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve   ## Set Inverse of a matrix (don't do it because this
                                                ## isn't the function who is really done in cacheSolve).
  getinverse <- function() inv                  ## Get the result of the function Inverse.
  list(set = set, get = get,                    ## The 4 functions created in the makeCacheMatrix environnement
       setinverse = setinverse,                 ## that can be called in the global environnement once the
       getinverse = getinverse)                 ## makeCacheMatrix has been assigned to an object in it.
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                         ## Get the value of the function Inverse if already done.
  if(!is.null(inv)) {                           ## Doesn't calculate again when called after the first time
                                                ## (when it already has a value).
    message("getting cached data")
    return(inv)
  }
  else {                                        ## When it doesn't already has a value, it calculates
                                                ## the inverse from here and gives it to 'inv'.
    data <- x$get()
    inv <- solve(data, ...)     
    x$setinverse(inv)
    inv
  }
}
