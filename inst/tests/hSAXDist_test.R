hs1<-matrix("FFEF68DE86B7B5256B94242332463A7666103DFC",ncol=1,nrow=1)
hs2<-matrix(c("FFEF68DE86B7B5256B94242332463A7666103DFC","8888888888888877777777777777778888888888"),ncol=1,nrow=2)
test_that('hsaxDist return zero distance to itself',{
  expect_equal(hsaxDist(hs1[1,1],hs1[1,1]),0)
  expect_equal(hsaxDist(hs2[2,1],hs2[2,1]),0)
  expect_false(0==hsaxDist(hs2[1,1],hs2[2,1]))
} )
