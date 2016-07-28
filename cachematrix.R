## This function calculate a matrix inverse using cache memory
## use cache memory reduces computational cost 

## This function return a list compound by functions,
## That will be called inside the next function.
## The functions "set" stored objects in chache memory. 
## The functions "get" search into objects stored.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  set.inverse<-function(matrix.inv) m<<-matrix.inv
  get.inverse<-function() m
  list(set=set, get=get,set.inverse=set.inverse,get.inverse=get.inverse)
  }


## This function uses the elements return by "makeCacheMatrix" and return the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$get.inverse()
  if(!is.null(m)){
    message("gettig inverse from cache data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$set.inverse(m)
  m
  }
