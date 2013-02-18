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
hashSAX<-function(cp,##<< signal 
  wl=16,##<< desired length of the string representation
  win=length(ts),##<< sliding window length. Signal will be represented as set of length(ts)-win+1 strings of wl characters each.
  verbose=FALSE##<< if TRUE print progress indicator
  ){
saxhash<-list()
ranlist<-data.frame(i=1,sax='sax',len=1,stringsAsFactors=FALSE)[FALSE,]
lastS<-hSAX(cp[1:win],wl,win)[1,1]
lastR<-1
len<- -1
l<-length(cp)-win
for(i in 1:l){
 wp<-cp[i:(i+win-1)]
 h<-hSAX(wp,wl,win)[1,1]
 if(h %in% names(saxhash)){
  saxhash[[h]][length(saxhash[[h]])+1]<-i
 }else{
  saxhash[[h]]<-list(i)
 }
 if(hsaxDist(h,lastS)==0){
  len<-len+1
  }else{
   if(len>0){
    ranlist[dim(ranlist)[1]+1,]<-list(lastR,lastS,len)
   }
   lastR<-i
   lastS<-h
   len<-0
  }
  if(verbose & (i %% 1000 ==0)){
   cat(paste(i,'\n'))
  }
 }
ret<-list(sax=saxhash,run=ranlist)
class(ret)<-'saxhash'
return(ret)
}
