iSAX<-function(
  ### Converts time series into iSAX representation and return object of class 'iSAX'
  ts,##<< time series to be analyzed
  card=4,##<< alphabet cardinality, number of bits time series will be coded with
  wl=16,##<< number of symbols. After conversion sliding window of the time series will be wl in length.
  win=length(ts),##<< window size. Time series will be converted into set of (l-win+1) strings 
  base##<< iSAXbase object (optional to increase speed of serial computations)
){
  if(length(card)>1){
    warning(sprintf('The card should be a single integer, length(card)=%d, only first element is going to be used',length(card)))
    card<-card[[1]]
  }
  if(is.na(as.numeric(card))){
    stop(paste('Card "',card,'" is not coercable to the integer'))
  }
  if(card!=as.integer(card)){
    warning(sprintf('There is a wrong value for card="%f", card supposed to be an integer in a range (0,8]\n card value is set to %d',card,as.integer(card)))
  }
  card<-as.integer(card)
  if(card<=0|card>8){
    warning(sprintf('There is a wrong value for card="%d", card supposed to be an integer in a range (0,8]\n card value is set to 4',card))
    card<-4
  }
  if(missing(base)) base<-iSAXbase(card)
  if(!any('iSAXbase'==class(base))) base<-iSAXbase(card)
  if(base$alphasize!=2^card) base<-iSAXbase(card)
  
  sig<-SAX.int(ts,alphasize=base$alphasize, wl=wl,win=win,base=base)
  res<-.int2iSAX(sig-1,card,base)
  return(res)
}

iSAX2str<-function(
### converts object of iSAX class into string  
  sig,##<< object to convert
  base##<< (optional) iSAXbase object
  ){
  if(class(sig)!='iSAX') stop('function require sig of class iSAX')
  if(missing(base)) base<-iSAXbase(card)
  if(!any('iSAXbase'==class(base))) base<-iSAXbase(card)
  if(base$alphasize!=2^card) base<-iSAXbase(card)
  if(class(sig)=='list'){
    return(lapply(sig,.iSAX2str,base=base))
  }
  return(.iSAX2str(sig,base))
}

.iSAX2str<-function(sig,base){
    return(paste(paste(base$alphabet[sig$sig+1],collapse=''),':',sig$card,sep=''))
}

int2iSAX<-function(
### function to convert zero-based set of integers into object of iSAX class  
  sig,##<< vector of integers in a range [0,2^card]
  card,##<< cardinality of the desired iSAX class
  base##<< (optional) object of class iSAXbase
){
  if(missing(base)) base<-iSAXbase(card)
  if(!any('iSAXbase'==class(base))) base<-iSAXbase(card)
  if(base$alphasize!=2^card) base<-iSAXbase(card)
  if(min(as.vector(sig))<0 | max(as.vector(sig))>=2^card){
    stop(sprintf('sig supposed to be a value in a range [0,%d]\n value in a range [%d,%d] provided.',
                 2^card-1,min(as.vector(sig)),max(as.vector(sig))))
  }
  if(class(sig)=='list'){
    return(lapply(sig,.int2iSAX,card=as.integer(card),base=base))
  }
  return(.int2iSAX(sig,as.integer(card),base))  
}

.int2iSAX<-function(sig,card,base){
  if(is.null(dim(sig))){
    res<-list(sig=as.integer(sig),card=card); 
    class(res)<-'iSAX'; 
  }else{
    res<-apply(sig,1,function(.x,.c){res<-list(sig=as.integer(.x),card=.c); class(res)<-'iSAX'; return(res)},card)
  }
  return(res)
}
cardUP<-function(){}
cardDOWN<-function(){}
