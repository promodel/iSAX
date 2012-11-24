.minDist<-function(alphasize){
  md<-matrix(0,ncol=alphasize,nrow=alphasize);
  bp <- c(-Inf,qnorm(1:(alphasize-1)/alphasize),Inf);
  for(i in 1:alphasize){
    j=i+2;
    while(j <=alphasize){
      #print(c(i,j,bp[i+1],bp[j]));
      md[i,j]<- abs(bp[i+1]-bp[j]);
      md[j,i]<- md[i,j];
      j=j+1;
    }
  }
  return(md)
}


hsaxDist<-function(
### distance between two hSAX strings  
  x,##<< first string
  y,##<< second string
  alphasize=16##<< expected alphabet size
  ){
  if(class(x)=="character"){
    xi<-hSAX2int(x)
  }else if(class(x)=="integer"){
    xi<-x
    if(min(xi)<1|max(xi)>16){
      stop(paste('X suppose to be in range [1,',alphasize,']'))
    }
  }else{
    stop('X suppose to be either string or vector of integers')
  }
  if(class(y)=="character"){
    yi<-hSAX2int(y)
  }else if(class(y)=="integer"){
    yi<-y
    if(min(yi)<1|max(yi)>16){
      stop(paste('Y suppose to be in range [1,',alphasize,']'))
    }
  }else {
    stop('Y suppose to be either string or vector of integers')
  }
  md<- .minDist(alphasize);
    dist<-sqrt(sum(apply(rbind(xi,yi),2,function(.x)md[.x[1],.x[2]])^2))
  return(dist)
}
