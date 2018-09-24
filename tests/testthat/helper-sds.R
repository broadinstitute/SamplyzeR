# example data for testing
#' Create a demo sample dataset with 5 Samples
#' @param z: optional, logical, whether include z-score, default True
#' @return SampleDataset object
#' @export
sdsExample <- function (z=T) {
  bamQcMetr = data.frame(
    'SampleID' = c('A', 'B', 'C', 'D', 'E'),
    'bam' = c(1, 2, 3, 4, 5))
  vcfQcMetr = data.frame(
    'SampleID' = c('A', 'B', 'C', 'D', 'E'),
    'vcf' = c(1, 2, 3, 4, 5))
  annot = data.frame(
    'SampleID' = c('A', 'B', 'C', 'D', 'E'),
    'Anno' = c('A1', 'A2', 'A3', 'A4', 'A5'))
  sds = sampleDataset(bamQcInput = bamQcMetr, vcfQcInpu = vcfQcMetr,
                      annotInput = annot, primaryID = 'SampleID')
  if(z) sds = calZscore(sds)
  return(sds)
}
