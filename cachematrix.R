## makeCacheMatrix() obtains a given matrix as the input object.
## cacheSolve() calculates the inverse of the given matrix and assign it to the cache;
## if the inverse has not been calculated and put into the cache yet,
## cacheSolve() will compute the inverse matrix and store it in the cache
## using makeCacheMatrix() after cacheSolve() clears any value of the inverse
## matrix that was cached by prior execution of cacheSolve(), so a new inverse matrix
## can be stored in the cache by executing cacheSolve() for the new input matrix subsequently
## Assuming all matrices used are invertible

## makeCacheMatrix sets value of matrix with set(); get value for x with get(), 
## sets inverse with setinverse(); get inverse with getinverse()
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL #initialise in function's environment, to use later
   set <- function(y) { #set value of matrix using this function; set() assigns the input argument to the x object in the parent environment
      x <<- y
      inv <<- NULL #NULL assigned to inv in parent environment; clears any value of inv that was cached by prior execution of cacheSolve()
   }
   
   get <- function() x ## gets value of matrix; retrieve x from parent env. of makeCacheMatrix()
   setinverse <- function(inverse) inv <<- inverse ## defines 'setter' for inv; set value of matrix
   getinverse <- function() inv ## defines 'getter' for inv; lexical scoping to find correct 'inv' to retrieve its value
   list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}

## cacheSolve() checks if inverse is already calculated
## if inverse already calculated, cache is not NULL value, get inverse from cache and skip inverse computation
## if inverse not already calculated, get matrix from input object
## then calculate inverse of matrix and set inverse matrix into cache with setinverse()
cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
      message("Getting cached data...")
      return(inv)
   }
   matrix <- x$get()
   inv <- solve(matrix, ...)
   x$setinverse(inv)
   inv  ## Return a matrix that is the inverse of 'x'
}
