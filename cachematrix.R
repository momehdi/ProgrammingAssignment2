## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix that is a list that contains a function to:
##           - Set the value of the matrix
##           - Get the value of the matrix
##           - Set the value of the inverse
##           - Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) inv<<- solve
  getinverse<-function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve computes the inverse of the matrix created using makeCacheMatrix.
## If the inverse has been already computed, it returns it from the cache.
## Otherwise, it computes the inverse of the matrix and set the value of the inverse
## in the cache using the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setmatrix(inv)
  inv
}
