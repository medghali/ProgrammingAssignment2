makeCacheMatrix<-function(x=matrix()){
  inverse<-NULL
  setmatrix<-function(y){
    x<<-y
    inverse<<-NULL
  }
  getmatrix<-function(){x}
  setinverse<-function(b){inverse<<-b}
  getinverse<-function() {inverse}
  list(setinverse=setinverse,getinverse=getinverse,getmatrix=getmatrix,getmatrix=getmatrix,setmatrix=setmatrix)
}
cacheSolve<-function(y){
  m<-y$getinverse()
  if (is.null(m)==FALSE){
    print('getting cached data')
    return(m)
  }
  else
  {
    matrice<-y$getmatrix()
    m<-solve(matrice)
    y$setinverse(m)
    return(m)
  
  }
}