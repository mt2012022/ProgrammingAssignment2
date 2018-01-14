## Put comments here that give an overall description of what your
## functions do
## Function to cache the inverse of a matrix -- objective
## Write a short comment describing this function
## make Cache Matrix creates a special matrix which is really a list containing a function to
#1.set the value of the matrix
#2.get the value of the matrix
#3 set the value of inverse of the matrix
#4 get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  get<- function() x
  setInv <- function(inverse) inv<<-inverse 
  getInv<- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function
## Creates the inverse of the special matrix created with the above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}


