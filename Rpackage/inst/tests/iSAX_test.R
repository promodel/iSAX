#library(iSAXr)
#library(testthat)
ts<-runif(720)
test_that('Check card validation',{
expect_that(iSAX(ts,card=-1),gives_warning())
expect_that(iSAX(ts,card=9),gives_warning())
expect_that(class(iSAX(ts,card=8)),matches('iSAX'))
expect_that(iSAX(ts,card=0.5),gives_warning())
expect_that(iSAX(ts,card=c(0.5,5)),gives_warning())
expect_that(iSAX(ts,card='s0.5'),throws_error())
})
#check dimensions after conversion
is5<-iSAX(ts,card=5,wl=30,win=100)
test_that('check dimensions after conversion',{
expect_that(class(is5),matches('list'))
expect_equal(621,length(is5))
expect_equal('iSAX',class(is5[[1]]))
expect_equal(5,is5[[1]]$card)
})
#check conversion
sig<-sin(2*pi*(0:99)/100)
s1<-c(2, 2, 2, 2, 2, 1, 1, 1, 1, 1)-1
s2<-c(3, 4, 4, 4, 3, 2, 1, 1, 1, 2)-1
s3<-c(6, 7, 8, 7, 6, 3, 2, 1, 2, 3)-1
s4<-c(11, 14, 15, 14, 11,  6,  3,  2,  3,  6)-1
s5<-c(21, 28, 30, 28, 22, 12,  5,  3,  5, 11)-1
s6<-c(42, 56, 59, 56, 44, 23,  9,  6,  9, 21)-1
s7<-c(84, 111, 118, 112,  88,  45,  18,  11,  17,  41)-1
s8<-c(167, 221, 235, 224, 175,  90,  36,  22,  33,  82)-1

test_that('Conversion values',{
  expect_equal(iSAX(sig,wl=10,card=1)$sig,s1)
  expect_equal(iSAX(sig,wl=10,card=2)$sig,s2)
  expect_equal(iSAX(sig,wl=10,card=3)$sig,s3)
  expect_equal(iSAX(sig,wl=10,card=4)$sig,s4)
  expect_equal(iSAX(sig,wl=10,card=5)$sig,s5)
  expect_equal(iSAX(sig,wl=10,card=6)$sig,s6)
  expect_equal(iSAX(sig,wl=10,card=7)$sig,s7)
  expect_equal(iSAX(sig,wl=10,card=8)$sig,s8)
})

test_that('int2iSAX conversion',{
  expect_equal(int2iSAX(s1,card=1),iSAX(sig,wl=10,card=1))
  expect_equal(int2iSAX(s2,card=2),iSAX(sig,wl=10,card=2))
  expect_equal(int2iSAX(s3,card=3),iSAX(sig,wl=10,card=3))
  expect_equal(int2iSAX(s4,card=4),iSAX(sig,wl=10,card=4))
  expect_equal(int2iSAX(s5,card=5),iSAX(sig,wl=10,card=5))
  expect_equal(int2iSAX(s6,card=6),iSAX(sig,wl=10,card=6))
  expect_equal(int2iSAX(s7,card=7),iSAX(sig,wl=10,card=7))
  expect_equal(int2iSAX(s8,card=8),iSAX(sig,wl=10,card=8))
})

lsig<-as.list(rep(list(iSAX(sig,wl=10,card=1)),7))
lint<-as.list(rep(list(s1),7))
test_that('conversion of lists',{
  expect_equal(int2iSAX(lint,card=1),lsig)
})

test_that('conversion to string',{
  expect_error(iSAX2str(s1))
  expect_equal(iSAX2str(int2iSAX(s1,card=1)),'1111100000:1')
})