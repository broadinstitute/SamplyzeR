library(samplyzer)
context("sampleDataset")

bamQcMetrDf <- data.frame('SampleID' = c("A", "B", "C"), 'bam' = c(1,2,3))
vcfQcMetrDf <- data.frame('SampleID' = c("A", "B", "C"), 'vcf' = c(1,2,3))
annotDf <- data.frame('SampleID' = c("A", "B", "C"),
                      'annot1' = c("A", "B", "C"))


test_that("Create a SampleDataset", {
  sds = sampleDataset(bamQcInput = bamQcMetrDf, vcfQcInput = vcfQcMetrDf,
                      annotInput = annotDf, primaryID = 'SampleID')
  expect_equal(sds$bamQcMetr, 'bam')
  expect_equal(sds$vcfQcMetr, 'vcf')
  expect_equal(class(sds), 'sampleDataset')
  expect_equal(sds$annot, 'annot1')
})


test_that("SampleDataset with only BAM QC metrics", {
  sds = sampleDataset(bamQcInput = bamQcMetrDf, annot = annotDf,
                      primaryID = 'SampleID')
  expect_equal(sds$bamQcMetr, 'bam')
  expect_equal(class(sds), 'sampleDataset')
  expect_equal(sds$annot, 'annot1')
})


test_that("SampleDataset with only VCF QC metrics", {
  sds = sampleDataset(vcfQcInput = vcfQcMetrDf, annot = annotDf,
                      primaryID = 'SampleID')
  expect_equal(sds$vcfQcMetr, 'vcf')
  expect_equal(class(sds), 'sampleDataset')
  expect_equal(sds$annot, 'annot1')
})

test_that("Create SampleDataset with predefined data.frame", {
  # order of the columns should be explictly defined to pass the unit test
  sds1 = list(
    df = data.frame('SampleID' = c("A", "B", "C"),
                    'annot1' = c("A", "B", "C"),
                    'bam' = c(1,2,3), 'vcf' = c(1,2,3), 'index' = c(1,2,3)),
    qcMetrics = c('bam', 'vcf'),
    bamQcMetr = 'bam', vcfQcMetr = 'vcf',  annot = 'annot1',
    primaryID = 'SampleID'
  )
  class(sds1) = 'sampleDataset'
  sds2 = sampleDataset(bamQcInput = bamQcMetrDf, vcfQcInput = vcfQcMetrDf,
                       annotInput = annotDf, primaryID = 'SampleID')
  expect_equal(sds1, sds2)
})

