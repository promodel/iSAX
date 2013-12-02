#library(iSAXr)
#library(testthat)
hs1<-matrix("FFEF68DE86B7B5256B94242332463A7666103DFC",ncol=1,nrow=1)
hs2<-matrix(c("FFEF68DE86B7B5256B94242332463A7666103DFC","8888888888888877777777777777778888888888"),ncol=1,nrow=2)
test_that('hSAX2int return results in proper dimentions',{
  ihs1<-hSAX2int(hs1)
  expect_equal(dim(ihs1),c(40,1))
  ihs2<-hSAX2int(hs2)
  expect_equal(dim(ihs2),c(40,2))
} )

