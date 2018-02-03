library(SamplyzeR)
context("SampleDataset")

bamQcMetr = data.frame('SampleID' = c("A", "B", "C"), 'bam' = c(1,2,3))
vcfQcMetr = data.frame('SampleID' = c("A", "B", "C"), 'vcf' = c(1,2,3))
annotations = data.frame('SampleID' = c("A", "B", "C"),
                         'annot' = c("A", "B", "C"))


test_that("Create a SampleDataset", {
  sds = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                      annotations = annotations, primaryID = 'SampleID')
  expect_equal(sds$bamQcMetr, 'bam')
  expect_equal(sds$vcfQcMetr, 'vcf')
  expect_equal(class(sds), 'sampleDataset')
  expect_equal(sds$annotations, 'annot')
})


test_that("SampleDataset with only BAM QC metrics", {
  sds = sampleDataset(bamQcMetr = bamQcMetr, annotations = annotations,
                      primaryID = 'SampleID')
  expect_equal(sds$bamQcMetr, 'bam')
  expect_equal(class(sds), 'sampleDataset')
  expect_equal(sds$annotations, 'annot')
})


test_that("SampleDataset with only VCF QC metrics", {
  sds = sampleDataset(vcfQcMetr = vcfQcMetr, annotations = annotations,
                      primaryID = 'SampleID')
  expect_equal(sds$vcfQcMetr, 'vcf')
  expect_equal(class(sds), 'sampleDataset')
  expect_equal(sds$annotations, 'annot')
})

test_that("Create SampleDataset with predefined data.frame", {
  # order of the columns should be explictly defined to pass the unit test
  sds1 = sampleDataset(
    df = data.frame('SampleID' = c("A", "B", "C"),
                    'annot' = c("A", "B", "C"),
                    'bam' = c(1,2,3),
                    'vcf' = c(1,2,3)),
    bamQcMetr = 'bam', vcfQcMetr = 'vcf', annotations = 'annot',
    primaryID = 'SampleID'
  )
  sds2 = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                       annotations = annotations, primaryID = 'SampleID')
  expect_equal(sds1, sds2)
})

test_that("Add outliers", {
  sds = sampleDataset(bamQcMetr = bamQcMetr, annotations = annotations,
                      primaryID = 'SampleID', outliers = c('A'))

})
