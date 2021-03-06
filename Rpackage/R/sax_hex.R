
getPAA <- function(
### Piecewise Aggregated approximation of the signal  
  ts, ##<< time series to be converted
  wl=16 ##<< number of symbols. After conversion time series will be wl in length.
  ){
	ts.n<-unlist(ts)
	if(class(ts.n)!='numeric'){
		stop(paste(ts,'\n not a numeric data'))
	}
	ts.n <- (ts.n-mean(ts.n))/sd(ts.n);
	l<-length(ts.n);
	if(l%%wl==0){
	  paa<-ts.n;
	  dim(paa)<-c(l/wl,wl);
	}else{
	  paa<-rep(ts.n,each=wl);
	  dim(paa)<-c(l,wl);
	}
	paa<-apply(paa,2,mean);
	return(paa);
### vector of doubles wl sy
}

cSAX <- function(
### Converts time series into SAX representation and return character string where SAX symbols mapped into letters of Latin alphabet for screen representation  
  ts,##<< time series to be analyzed
  alphasize=4,##<< alphabet cardinality, number of levels time series will be split on
  wl=16,##<< number of symbols. After conversion sliding window of the time series will be wl in length.
  win=length(ts),##<< window size. Time series will be converted into set of (l-win+1) strings 
  base##<< SAXbase object
){
  if(missing(base)) base<-.SAXbase(alphasize)
  if(base$alphasize!=alphasize) base<-.SAXbase(alphasize)
  ### alphasize value overwrites base value if base$alphasize do not correspond to alphasize provided
  bp <- base$bp;
  alphabet <-base$alphabet
#	alphabet <- ordered(letters[1:alphasize],levels=letters[1:alphasize]);
#	bp <- c(-Inf,qnorm(1:(alphasize-1)/alphasize),Inf);
	l<-length(ts);
	i1=rep(1:(l-win+1),win);
	c(l-win+1,win)->dim(i1);
	i2=rep(0:(win-1),(l-win+1));
	c(win,l-win+1)->dim(i2);
	i2=t(i2);
	i1=i1+i2;
	rm(i2);
	ts.sl<-ts[i1];
	dim(i1)->dim(ts.sl);
	paa <- apply(ts.sl,1,function(.x) getPAA(.x,wl));
	sig <- apply(paa,1, function(.x) cut(.x,bp,labels=alphabet));
#	sig <- apply(sig,1,function(.x) paste(.x,collapse=''))
	rm(paa);
	return(sig);
### Latin representation of the signal SAX string
}

SAX.int <- function(
  ### Converts time series into SAX representation and return character string where SAX symbols mapped into integer levels
  ts,##<< time series to be analyzed
  alphasize=4,##<< alphabet cardinality, number of bits time series will be coded with
  wl=16,##<< number of symbols. After conversion sliding window of the time series will be wl in length.
  win=length(ts),##<< window size. Time series will be converted into set of (l-win+1) strings 
  base##<< SAXbase object
){
  if(missing(base)) base<-.SAXbase(alphasize)
  if(base$alphasize!=alphasize) base<-.SAXbase(alphasize)
### alphasize value overwrites base value if base$alphasize do not correspond to alphasize provided
  bp <- base$bp;
	l<-length(ts);
	i1=rep(1:(l-win+1),win);
	c(l-win+1,win)->dim(i1);
	i2=rep(0:(win-1),(l-win+1));
	c(win,l-win+1)->dim(i2);
	i2=t(i2);
	i1=i1+i2;
	rm(i2);
	ts.sl<-ts[i1];
	dim(i1)->dim(ts.sl);
	paa <- apply(ts.sl,1,function(.x) getPAA(.x,wl));
	sig <- apply(paa,1, function(.x) cut(.x,bp,labels=FALSE));
	rm(paa);
	return(sig);
### integer representation of SAX signal
}

.SAXbase<-function(alphasize=4){
  letters <- c( "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z");
  alphabet <- ordered(letters[1:alphasize],levels=letters[1:alphasize]);
  bp <- c(-Inf,qnorm(1:(alphasize-1)/alphasize),Inf);
  base<-list(alphasize=alphasize,alphabet=alphabet,bp=bp)
  class(base)<-'SAXbase'
  return(base)
}

.SAX <- function(ts,wl=16,win=48,base){
	l<-length(ts)-win+1;
	sig=rep('',l);
	c(l,1)->dim(sig)
  alphabet<-base$alphabet
  bp<-base$bp
	for(i in 1:l){
	if(i%%100==0) {print(i)}
	paa <- getPAA(ts[i:(i+win-1)],wl);
	sig[i,] <- paste(cut(paa,bp,labels=alphabet),collapse='');
	}
	return(sig);
}

hSAXbase<-function(
### define basic properties of hexSAX string:
### size of the alphabet, alphabet itself  and break points for convertison
  alphasize=16##<< size of the alphabet, should be less then 16
  ){
  if(alphasize>16) stop('size of the alphabet could not be larger then 16')
  alphabet <- ordered(c('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'),levels=c('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'));
  bp <- c(-Inf,qnorm(1:(alphasize-1)/alphasize),Inf);
  
  base<-list(alphasize=alphasize,alphabet=alphabet,bp=bp)
  class(base)<-'SAXbase'
  return(base)
}

iSAXbase<-function(
  ### define basic properties of iSAX string:
  ### the cardinality of the alphabet, size of the alphabet, 
  ### alphabet itself  and break points for convertison
  card=8##<< cardinality of the alphabet, should be less then 8
){
  if(card>8) stop('cardinality of the alphabet could not be larger then 8')
  alphasize<-2^card
  if(alphasize<=16){
    alphabet <- ordered(c('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'),levels=c('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'));
  }else{
    alphabet <- ordered(c(
      '00','01','02','03','04','05','06','07','08','09','0A','0B','0C','0D','0E','0F',
      '10','11','12','13','14','15','16','17','18','19','1A','1B','1C','1D','1E','1F',
      '20','21','22','23','24','25','26','27','28','29','2A','2B','2C','2D','2E','2F',
      '30','31','32','33','34','35','36','37','38','39','3A','3B','3C','3D','3E','3F',
      '40','41','42','43','44','45','46','47','48','49','4A','4B','4C','4D','4E','4F',
      '50','51','52','53','54','55','56','57','58','59','5A','5B','5C','5D','5E','5F',
      '60','61','62','63','64','65','66','67','68','69','6A','6B','6C','6D','6E','6F',
      '70','71','72','73','74','75','76','77','78','79','7A','7B','7C','7D','7E','7F',
      '80','81','82','83','84','85','86','87','88','89','8A','8B','8C','8D','8E','8F',
      '90','91','92','93','94','95','96','97','98','99','9A','9B','9C','9D','9E','9F',
      'A0','A1','A2','A3','A4','A5','A6','A7','A8','A9','AA','AB','AC','AD','AE','AF',
      'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9','BA','BB','BC','BD','BE','BF',
      'C0','C1','C2','C3','C4','C5','C6','C7','C8','C9','CA','CB','CC','CD','CE','CF',
      'D0','D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB','DC','DD','DE','DF',
      'E0','E1','E2','E3','E4','E5','E6','E7','E8','E9','EA','EB','EC','ED','EE','EF',
      'F0','F1','F2','F3','F4','F5','F6','F7','F8','F9','FA','FB','FC','FD','FE','FF'
      ),levels=c(
        '00','01','02','03','04','05','06','07','08','09','0A','0B','0C','0D','0E','0F',
        '10','11','12','13','14','15','16','17','18','19','1A','1B','1C','1D','1E','1F',
        '20','21','22','23','24','25','26','27','28','29','2A','2B','2C','2D','2E','2F',
        '30','31','32','33','34','35','36','37','38','39','3A','3B','3C','3D','3E','3F',
        '40','41','42','43','44','45','46','47','48','49','4A','4B','4C','4D','4E','4F',
        '50','51','52','53','54','55','56','57','58','59','5A','5B','5C','5D','5E','5F',
        '60','61','62','63','64','65','66','67','68','69','6A','6B','6C','6D','6E','6F',
        '70','71','72','73','74','75','76','77','78','79','7A','7B','7C','7D','7E','7F',
        '80','81','82','83','84','85','86','87','88','89','8A','8B','8C','8D','8E','8F',
        '90','91','92','93','94','95','96','97','98','99','9A','9B','9C','9D','9E','9F',
        'A0','A1','A2','A3','A4','A5','A6','A7','A8','A9','AA','AB','AC','AD','AE','AF',
        'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9','BA','BB','BC','BD','BE','BF',
        'C0','C1','C2','C3','C4','C5','C6','C7','C8','C9','CA','CB','CC','CD','CE','CF',
        'D0','D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB','DC','DD','DE','DF',
        'E0','E1','E2','E3','E4','E5','E6','E7','E8','E9','EA','EB','EC','ED','EE','EF',
        'F0','F1','F2','F3','F4','F5','F6','F7','F8','F9','FA','FB','FC','FD','FE','FF'
      ));
    
  }
  bp <- c(-Inf,qnorm(1:(alphasize-1)/alphasize),Inf);
  
  base<-list(alphasize=alphasize,alphabet=alphabet[1:alphasize],bp=bp)
  class(base)<-c('iSAXbase','SAXbase')
  return(base)
}


hSAX<-function(
### convert signal into hSAX string  
  ts,##<< signal 
  wl=16,##<< desired length of the string representation
  win=length(ts),##<< sliding window length. Signal will be represented as set of length(ts)-win+1 strings of wl characters each.
  base##<< SAXbase object to speeed up calculations.
  ){
  if(missing(base)) base<-hSAXbase()
  sig<-.aSAX(ts,wl=wl,win=win,base)
  return(sig);
}

.aSAX<-function(ts,wl=16,win=48,base){
	bp <- base$bp;#c(-Inf,qnorm(1:(alphasize-1)/alphasize),Inf);
	alphabet<-base$alphabet;
	l<-length(ts)-win+1;
	sig=rep('',l);
	c(l,1)->dim(sig)
	for(i in 1:l){
		
		paa <- getPAA(ts[i:(i+win-1)],wl);
		sig[i,] <- paste(cut(paa,bp,labels=alphabet),collapse='');
	}
	return(sig);
}

.cSAX2int<-function(sig,base){#,alphasize=4){
#	letters <- c( "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z");
#	alphabet <- ordered(letters[1:alphasize],levels=letters[1:alphasize]);
#	bp <- c(-Inf,qnorm(1:(alphasize-1)/alphasize),Inf);
	ns<-length(sig)
	l<-nchar(sig[1])
	c<-strsplit(sig,'')
	match(unlist(c),base$alphabet)->ic
	dim(ic)<-c(l,ns)
	return(t(ic))
	
}

hSAX2int<-function(
### convert the hSAX string into vector of integers  
  str,##<< hSAX string
  base##<< object of class 'SAXbase', optional 
  ){
  if(missing(base)) base<-hSAXbase()
  if(class(base)!='SAXbase'){
    stop('base should be of class "SAXbase"')
  }
#  alphasize<-base$alphasize
#  alphabet <- base$alphabet
#  bp <- base$bp
  ns<-length(str)
  l<-nchar(str[1])
  c<-strsplit(str,'')
  match(unlist(c),base$alphabet)->ic
  dim(ic)<-c(l,ns)
  return((ic))
  
}

.cSAX2double<-function(str,base){
  ns<-length(str)
  l<-nchar(str[1])
  is<-.cSAX2int(str,base);
  bp <- qnorm(1:(base$alphasize)/(base$alphasize+1));
  ds<-base$bp[is]
  
}

hSAX2double<-function(
### convert the hSAX string into vector of doubles  
  str,##<< hSAX string
  base##<< object of class 'SAXbase', optional 
){
  if(missing(base)) base<-hSAXbase()
  if(class(base)!='SAXbase'){
    stop('base should be of class "SAXbase"')
  }
  ns<-length(str)
  l<-nchar(str[1])
  is<-hSAX2int(str,base=base);
  ds<-base$bp[is]
}

hSAX2signal<-function(
### function return double representation of the hSAX signal of the same width as original signal
  str,##<< hSAX string
  win=48##<< width of the original signal
  ){
  ppl<-win/nchar(str[1])
  ds<-hSAX2double(str)
  return(rep(ds,each=ppl))
}
