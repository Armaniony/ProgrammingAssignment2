## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creating a special "matrix" object 
## that can cache its inverse can be done by:
## First setting the input x as a matrix
## Then setting the inverse value "inv" as a null
## and finally changing all references to "mean" to "inverse"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function()inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## In order to inverse of the special "matrix"
## returned by makeCacheMatrix 
## we can change "mean" to "inverse" and "m" to "inv"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Getting Inversed Matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinv(inv)
  inv
}