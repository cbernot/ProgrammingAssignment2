##calculating the inverse of a square matrix
##create an object to store a cache value of the inverse
##define functions and return a list of functions used as input into CacheSolve
##These functions are:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse in cache
##get the value of the inverse in cache


makeCacheMatrix <- function(x = matrix()) {
    
    
    invm<-NULL
    
    set<-function(y){
        x<<-y
        invm<<-NULL
          ## is used to assign a value to an object that exists in a different 
          ##environment 
          
    }
    
    get<-function()x
    
    setinverse<-function(inverse) invm<<-inverse
    getinverse<-function() invm
    list(set=set,get=get,
        setinverse=setinverse,
        getinverse=getinverse)
    }
    



## return a matrix that is the inverse of "x" matrix input from makeCacheMatrix
##this function assumes that the matrix supplied is always invertible


##list invm

cacheSolve <- function(x, ...) {
  ##access the inverse of the matrix
  ##place condition that if invm is not a null value the return message "getting 
  ##cached data"
  ## then access the values in matrix x 
  
  invm<-x$getinverse()
  
  if(!is.null(invm)){
    message("getting cached data")
    return(invm)}
  
  ## if invm is NULL then:
  ##calculate the inverse
  
  
    matrixd<-x$get()
    invm<-solve(matrixd, ...)
    ##now store in cache the inverse value 
    
    
    x$setinverse(invm)
    
    return(invm)
    
  }
  



 ##I have included matrix values to test makeCacheMatrix and cacheSolve   
    
a<-makeCacheMatrix()
a$set(matrix(1:4,2,2))
a$get()


cacheSolve(a)





