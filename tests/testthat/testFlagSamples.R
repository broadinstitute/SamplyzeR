library(samplyzer)
context("FlagSamples")

# test flagSamples.default
test_that("Flag samples in a data.frame", {
  # test dfs
  df = data.frame(
    'sample_id' = c('A', 'B', 'C', 'D'),
    'stat' = c(1, 5, 10, 20), stringsAsFactors = F)
  df2 = data.frame(
    'sample_id' = c('A', 'B', 'C', 'D'), 'stat' = c(1, 5, 10, 20),
    'flaggedReason' = c('flag1', '', 'flag3', ''), stringsAsFactors = F)

  # add
  expect_df1 = data.frame(
    'sample_id' = c('A', 'B', 'C', 'D'),
    'stat' = c(1, 5, 10, 20),
    'flaggedReason' = c('','','stat>5','stat>5'),
    stringsAsFactors = F)
  test_out_df1 = flagSamples(df, 'stat', 5, greater=T)
  expect_equal(test_out_df1, expect_df1)

  expect_df2 = data.frame(
      'sample_id' = c('A', 'B', 'C', 'D'),
      'stat' = c(1, 5, 10, 20),
      'flaggedReason' = c('stat<=9','stat<=9', '', ''),
      stringsAsFactors = F)
  test_out_df2 = flagSamples(df, 'stat', 9, greater=F)
  expect_equal(test_out_df2, expect_df2)

  # test case: add flag to both existing and non-existing flags
  test_out = flagSamples(df2, 'stat', 9, greater=F)
  expect_out = data.frame(
    'sample_id' = c('A', 'B', 'C', 'D'), 'stat' = c(1, 5, 10, 20),
    'flaggedReason' = c('flag1,stat<=9', 'stat<=9', 'flag3', ''),
    stringsAsFactors = F)
  expect_equal(test_out, expect_out)

})

# test flagSamples.dataset
test_that("Flag samples from a sampleDataset", {
  cutoffs = data.frame(
    qcMetrics = c('bam', 'vcf'), greater = c(T, F), values = c(2, 3))

  # expectFlagReason = c('', '', '', '')
  # sds = flagSamples(sdsExample(), cutoffs)
  # expect_equal(sds$df$flaggedReason, expectFlagReason)

  # test: Flag samples of a SampleDataset with zscore

  test_out = flagSamples(sdsExample(), cutoffs, zscore=0.5)
  expect_out = data.frame(
    flaggedReason = c('vcf<=3,bamZscore<=-0.5,vcfZscore<=-0.5',
                      'vcf<=3,bamZscore<=-0.5,vcfZscore<=-0.5',
                      'bam>2,vcf<=3',
                      'bam>2,bamZscore>0.5,vcfZscore>0.5',
                      'bam>2,bamZscore>0.5,vcfZscore>0.5'),
    stringsAsFactors = F)
  expect_equal(
    getAttr(flagSamples(sdsExample(), cutoffs, zscore=0.5), 'flaggedReason'),
    expect_out)
})
