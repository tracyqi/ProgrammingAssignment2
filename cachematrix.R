## Put comments here that give an overall description of what your
## functions do

## This function provides basic operations to make a cached matrix inverse. 
## 1. set
## 2. get
## 3. setFunc
## 4. getFunc
makeCacheMatrix <- function(x = matrix()) 
{
  # initialize
  m<-NULL
  
  # set value
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  # get value
  get<-function() x
  
  # set operation
  setFunction <- function(Op) m  <<- Op
  
  # get operation results
  getFunction <- function() m
  
  list(set=set, 
       get=get,
       setFunc=setFunction,
       getFunc=getFunction)
}

## This function will use makeCacheMatrix function to solve a matrix
cacheSolve <- function(x=matrix(), ...) 
{
  # try to retrieve a cached matrix if possible, if it is not null, we will just return the cached version
  m<-x$getFunc()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # we will have to solve it as we didn't find it in cache
  matrix<-x$get()
  
  # make sure the cache is invertible
  if(det(matrix)!=0)
  {
    m<-solve(matrix, ...)
    x$setFunc(m)
    return(m)
  }
  else
  {
    message("matrix is not invertible")
  }
}
