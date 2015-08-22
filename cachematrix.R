## Together the functions cache the inverse of a matrix
## This saves processing time

## This function takes a matrix x and returns
## a list to pass to the the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
          i<-NULL
          set<-function(y){
                    x<<-y
                    i<<-NULL
          }
          get<-function()x
          setinverse<-function(inverse)i<<-inverse
          getinverse<-function()i
          list(set=set,
               get=get,
               setinverse=setinverse,
               getinverse=getinverse)
}

## This function takes  a list created by the
## makeCacheMatrix function as its argument
## If the inverse of the matrix has already been calculated
## then cacheSolve function gets the inverse from the cache
## otherwise it computes the inverse in the cache

cacheSolve <- function(x, ...) {
          i<-x$getinverse()
          if(!is.null(i)){
                    message("getting cached data")
                    return(i)
          }
          data<-x$get()
          i<-solve(data,...)
          x$setinverse(i)
          i
}
