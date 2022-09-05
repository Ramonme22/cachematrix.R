#Create functions (makecachematrix and cachematrixsolve), that cache an inverse of a matrix

#makecachematrix is a function that creates a matrix and save the inverse of its input
makecachematrix<-function(x=matrix()){
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function()x
  setinv<-function(inv)inverse<<-inv
  getinv<-function()inverse
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}
# cachematrixsolve is a function that computes the inverse of the matrix that the first function creates
cachematrixsolve<-function(x,...){
  inverse<- x$getinv()
  if(!is.null(inverse)){
    message("getting data")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data,...)
  x$setinv(inverse)
  inverse
}
