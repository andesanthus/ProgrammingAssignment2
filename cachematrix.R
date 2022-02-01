## These functions create an object that can store a matrix and its inverse and also a function that calculate and assigns the inverse if it has not been calculated before.

## This function creates an object where can be set the value of a matrix, get its value, set the inverse of the matrix and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setinv<-function(inv.cal){inv<<-inv.cal}
  getinv<-function(){inv}
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function searchs the inverse of the matrix on the argument, if its calculated, returns it, if not, it calculates it.

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}