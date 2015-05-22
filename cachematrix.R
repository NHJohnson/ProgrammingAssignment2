## These two functions jointly permit caching of the inverse of a 
## matrix so that it does not need to be recalculated if needed 
## more than once. 

## This first function takes a matrix as its argument and returns a
## list that functions in many respects as a matrix but actually 
## consists of four functions which permit the matrix to be stored
## (set) and retrieved (get), and likewise for its inverse
## (setinv and getinv).

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
    inv <<- NULL
	}
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes as its argument an object of the type 
## created by makeCacheMatrix above. If the supplied object already
## has an inverse calculated and stored it returns this inverse; 
## otherwise it computes the inverse, stores it in the object via
## setinv, and returns it. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message('getting cached data')
    return(inv)
  } 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
