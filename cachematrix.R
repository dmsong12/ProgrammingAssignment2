## These 2 functions will create a special matrix, and cashe its inverse matrix so it need not be computed everytime its needed

## This function will create a special matrix from the input values
## it will set the values of the matrix
## it will get the values of the matrix
## it will set the values of the inverse matrix
## it will get the value of the inverses matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){x}
  
  setinverse <- function(solvmatrix) {inv <<- solvmatrix}
  
  getinverse <- function() {inv}
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function will return a the inverse of the input if it is available already (already cached in the function)
## if not it will get the values and find the inverse matrix, set it in the cashe, and return the values.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv      
}
