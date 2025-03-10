# The caMatrix function creates a special matrix that can cache its inverse
CaMatrix<- function(x=matrix()){
  inv<- NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
    }
  #To get the matrix
  get<-function() x
  #To set the inverse
  setinverse<-function(inverse)inv<<-inverse

  #To get the inverse
  getinverse<-function() inv

  #To return the list of functions
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  }
#The Cacompute function computes the inverse of the matrix previously created returned by Camatrix 
# it also retrieves the cache inverse if it has already been calculated

Cacompute<-function(x,...){
  inv<-x$getinverse() #this checks if inverse is already cached
  if(!is.null(inv)){
    return(inv)
    }
  matrix2<-x$get() #to retrieve the matrix
  inv<-solve(matrix2,...) #to compute the inverse
  x$setinverse(inv) #to cache the inverse
  inv
  }

