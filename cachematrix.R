## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse,

makeCacheMatrix <- function(x = matrix()) {
 I<-NULL
  set<-function(y){
    x<<-y
    m<-NULL
  }
  get<-function(){x}
  setI<-function(Inverse){I<<-Inverse}
  getI<-function(){I}
  list(set=set,get=get,setI=setI,getI=getI)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
 I<-x$getI()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data<-x$get()
  I<-solve(data,...)
  x$setI(I)
  I
}
