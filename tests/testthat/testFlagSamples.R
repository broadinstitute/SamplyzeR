library(SamplyzeR)
context("FlagSamples")

# test flagSamples.default
test_that("Flag samples in a data.frame", {
  df = data.frame('sample_id' = c('A', 'B', 'C', 'D'),
                  'stat' = c(1, 5, 10, 20), stringsAsFactors = F)

  expect_df1 = data.frame(
    'sample_id' = c('A', 'B', 'C', 'D'),
    'stat' = c(1, 5, 10, 20),
    'flaggedReason' = c(NA,NA,'stat>5,','stat>5,'),
    stringsAsFactors = F)
  test_out_df1 = flagSamples(df, 'stat', 5, greater=T)
  expect_equal(test_out_df1, expect_df1)

  expect_df2 = data.frame(
      'sample_id' = c('A', 'B', 'C', 'D'),
      'stat' = c(1, 5, 10, 20),
      'flaggedReasons' = c('stat<=9,','stat<=9,', NA, NA),
      stringsAsFactors = F)
  test_df2 = flagSamples(df, 'stat', 9, greater=F)

  expect_equal(test_out_df2, expect_df2)
})

# test flagSamples.dataset
cutoffs = data.frame(qc = c('bam', 'vcf'), greater = c(T, F),
                     values = c(2, 3))

test_that("Flag samples from a sampleDataset", {
  expectFlagReason = c('', '', '', '')
  sds = flagSamples(.sds(), cutoffs)
  expect_equal(sds$df$flaggedReason, expectFlagReason)
})

test_that("Flag samples of a SampleDataset with zscore", {
  expectFlagReason = c('', '', '')
  expect_equal(getAttr(flagSamples(.sds(), cutoffs, zscore=4), 'flaggedReason'),
               expectFlagReason)
})
