library(samplyzer)
context('S3Method')

test_that("Save SDS", {
  save(sdsExample(z=F), tsv='sdsExample.tsv')
})
