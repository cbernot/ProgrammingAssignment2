##calculating the inverse of a square matrix
##first portion of program is to fill the cache of the matrix:
##set a value of a matrix to NULL
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse in cache
##get the value of the inverse in cache


makeCacheMatrix <- function(x = matrix()) {
    
    
    invm<-NULL
    
    set<-function(y){
        x<<-y
        invm<<-NULL
    }
    
    get<-function()x
    
    setinverse<-function(inverse) invm<<-inverse
    getinverse<-function() invm
    list(set=set,get=get,
        setinverse=setinverse,
        getinverse=getinverse)
    }
    



## return a matrix that is the inverse of "x"
##this function assumes that the matrix supplied is always invertible
##access the inverse of the matrix
##place condition that if invm is not a null value the return message "getting 
##cached data"
## then access the values in matrix x
## if invm is NULL then:
##calculate the inverse
##now store in cache the inverse value
##list invm

cacheSolve <- function(x, ...) {
  
  invm<-x$getinverse()
  
  if(!is.null(invm)){
    message("getting cached data")
    return(invm)}
    
    matrixd<-x$get()
    invm<-solve(matrixd, ...)
    
    x$setinverse(invm)
    return(invm)
    
  }
  



    
    
a<-makeCacheMatrix()
a$set(matrix(1:4,2,2))
a$get()


cacheSolve(a)





