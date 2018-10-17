library(samplyzer)
context('testUtils')

test_that("LoadInput with data.frame",{
  df = loadInput(bamQcMetrDf)
  expect_true(all(
    is.data.frame(df),
    df$SampleID == c('A', 'B', 'C'),
    df$bam == c(1, 2, 3)))
})


test_that("LoadInput with tsv", {
  df = loadInput('data/bamQc.tsv')
  expect_true(all(
    is.data.frame(df),
    df$SampleID == c('A', 'B', 'C'),
    df$bam == c(1, 2, 3)))
})
