## These functions calculate the inverse of a matrix and cache
## the result so that repeated calculations are not necessary.

## The makeCacheMatrix function creates a matrix obect, and 
## establishes the function that operate on that matrix.

## The function consist of a 'set' function that sets the value
## of the matrix, a 'get' function that retrieves the value of 
## matrix, a 'setinverse' function that sets the value of the 
## inverse of the matrix, and a 'getinverse' function that 
## retreives the value of the inverse if it has already been
## calculated.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL         # Set inverse to NULL
      
      ## Establish 'set' function for matrix
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      
      ## Establish 'get' function for matrix
      get <- function() x
      
      ## Establish the 'setinverse' function to calculate the inverse 
      ## and store the value 
      setinverse <- function(solve) inverse <<- solve
      
      ## Establish the 'getinverse' function to retrieve the store
      ## inverse if it has already been calculated
      getinverse <- function() inverse
      
      list(set = set, get = get, setinverse = setinverse, 
           getinverse = getinverse)
}


## The cacheSolve function retrieves the calculated inverse of the
## matrix if it exists. Otherwise, the function calls the solve()
## function to calculate the inverse and store it if it does not
## already exist.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      ## Attempt to retrieve a previously calculated inverse
      inverse <- x$getinverse()
      
      ## If the value of the inverse is found, retreive the 
      ## stored value
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      
      ## If the value of the inverse does not already exist,
      ## then call the solve() function to calculate the inverse
      ## of the matrix
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
