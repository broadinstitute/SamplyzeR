library(samplyzer)
context("SampleDataset")

bamQcMetr = data.frame('SampleID' = c("A", "B", "C"), 'bam' = c(1,2,3))
vcfQcMetr = data.frame('SampleID' = c("A", "B", "C"), 'vcf' = c(1,2,3))
annotations = data.frame('SampleID' = c("A", "B", "C"),
                         'Anno' = c("A", "B", "C"))


test_that("Create a SampleDataset", {
  sds = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                      annotations = annotations, primaryID = 'SampleID')
  expect_equal(sds$bamQcMetr, 'bam')
  expect_equal(sds$vcfQcMetr, 'vcf')
  expect_equal(class(sds), 'sampleDataset')
  expect_equal(sds$annotations, 'Anno')
})

test_that("SampleDataset with only BAM QC metrics", {
  sds = sampleDataset(bamQcMetr = bamQcMetr, annotations = annotations,
                      primaryID = 'SampleID')
  expect_equal(sds$bamQcMetr, 'bam')
  expect_equal(class(sds), 'sampleDataset')
  expect_equal(sds$annotations, 'Anno')
})

test_that("SampleDataset with only VCF QC metrics", {
  sds = sampleDataset(vcfQcMetr = vcfQcMetr, annotations = annotations,
                      primaryID = 'SampleID')
  expect_equal(sds$vcfQcMetr, 'vcf')
  expect_equal(class(sds), 'sampleDataset')
  expect_equal(sds$annotations, 'Anno')
})
