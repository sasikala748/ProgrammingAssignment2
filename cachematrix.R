## The makeCachematrix creates a matrix that could cache its inverse
## get the matrix to inverse

## the function stores the matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-null
  set<-function(y){
      x<<-y
      inv<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)inv<<-inverse
  getInverse<-function()inv
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## This cacheSolve function gives inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
      message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
