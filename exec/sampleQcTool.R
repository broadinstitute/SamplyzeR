#!/usr/bin/env Rscript
devtools::install_github('xiaolicbs/samplyzer')  # Stay current version
library(argparse)
library(samplyzer)

createSds <- function(bamQcInput, vcfQcInput, annotTsv, stratify, primaryID){
  stratify = unlist(strsplit(stratify, ','))
  sds = sampleDataset(annotTsv, primaryID, bamQcInput, vcfQcInput, stratify)
}

qcMetrScatterPlots <- function(sds, stratify, prefix){
  grobList = sampleQcPlot(sds, qcMetrics=sds$bamQcMetr, annotation = stratify,
                          geom = 'scatter', ncols = 2, show = F)
  ggplot2::ggsave(paste(prefix, stratify,'QcMetrScatter.pdf', sep = '.'),
                  grobList, width = 12, height = 15)
  return(TRUE)
}

cutoffDf <- function (cutoffTsv) {
  if (is.null(cutoffTsv)) {
    cutoffs = data.frame(
      qcMetrics = c('Percent_lt_30bp_frags', 'Contamination_Estimation',
                    'Percent_Chimeric_Reads'),
      value = c(0.01, 0.02, 0.05),
      greater = c(T, T, T),
      stringsAsFactors = F
    )
  } else {
    cutoffs = read.csv(opt$cutoffs, sep = '\t')
  }
  return(cutoffs)
}

stratfityAnalysis <- function(sds, stratify, cutoffDf) {
  cat(" o Perform a stratified QC metrics analysis\n")
  # Calculate z-scores for each batch across different ancestry
  sds = calZscore(sds, strat=c(stratify,'inferredAncestry'),
                  qcMetrics = .stratQcMetrGATK())

  cat(" o Filter samples with hard cutoffs and z-scores\n")
  sds = flagSamples(sds, cutoffs=cutoffDf, zscore=4)
  return(sds)
}

pcaAnalysis <- function(sds, samplePc, refPC) {
  if (!all(is.null(refPc), is.null(samplePc))) {
    cat(" o Load Sample and Reference genotype PCs\n")
    samplepc = read.csv(opt$samplepc, sep = '\t')
    refpc = read.csv(opt$refpc, sep = '\t')

    cat(" o Add genotype PCs to sample Dataset\n")
    sds = setAttr(sds, attributes = 'PC', data = samplepc,
                  primaryID = opt$primaryID)

    cat(" o Infer ancestry from PCA results\n")
    sds = inferAncestry(sds, trainSet = refpc[,c('PC1', 'PC2', 'PC3')],
                        knownAncestry = refpc$knownAncestry)
  }
  return(sds)
}

sampleQc <- function (
  annotTsv, primaryID, stratify, prefix, bamQcTsv, vcfQcTsv, cutoffTsv,
  samplePc, refPc, zscore) {
  sds = sampleDataset(bamQcInput, vcfQcInput, annotTsv, stratify, primaryID)
  # qcMetrScatterPlots(sds, stratify, prefix)
  #
  # # analysis indicator
  # ifHardCutoff = FALSE; ifPca = FALSE; ifZscore = FALSE
  # if (!is.null(cutoffTsv)) { ifHardCutoff = TRUE }
  # if (!is.null(samplePc) & !is.null(refPc)){ ifPca = TRUE }
  # if (ifHardCutoff & ifPca){ ifZscore=TRUE }
  #
  # if (ifHardCutoff) {
  #   cutoffDf = cutoffDf(cutoffTsv)
  #   sds = flagSamples(sds, cutoffs=cutoffDf)
  # }
  # if (ifPca) { sds = pcaAnalysis(sds, samplePc, refPc) }
  # if (ifZscore) { sds = stratifyAnalysis(sds, stratify, cutoffDf) }
  # save(sds, RDS=paste(prefix, '.RDS', sep=''), tsv=paste(prefix,'.tsv',sep=''),
  #      xls=paste(prefix, '.xls', sep=''))
  # cat("Save BAM QC plot to pdf.\n")
}

# main
toolDescr = paste('This tool performs routine sample QC process, including',
'sample QC plots, PCA plots, flag outliers based on certain cutoffs.')

parser <- ArgumentParser(description=toolDescr)

# required arguments
parser$add_argument(
  "-a", "--annotTsv", required=TRUE, help = "A sample annotation file tsv")
parser$add_argument(
  "-d", "--primaryID", required=TRUE,
  help = "Name of primary IDs in input TSVs to specific each sample")
parser$add_argument(
  "-s", "--stratify", required=TRUE,
  help=paste("A list of attributes used in the stratified QC metrics analysis",
             "separated by comma"))
parser$add_argument(
  "-p", "--prefix", required=TRUE, help = "Prefix of output files")

# optional arguments
parser$add_argument(
  "-b", "--bamQcTsv", default = NULL,
  help = "A tsv for BAM Level quality control metrics")
parser$add_argument(
  "-v", "--vcfQcTsv", default = NULL,
  help = "A tsv for VCF Level quality control metrics")
parser$add_argument(
  "--cutoffTsv", default = NULL,
  help = 'a tsv file that contains hard cutoffs for QC metrics')
parser$add_argument(
  "-g", "--samplePc", default = NULL,
  help = 'A tsv file with genotype PCs for each sample')
parser$add_argument(
  "-r", "--refPc", default = NULL,
  help = 'A tsv file with genotype PCs from a reference set of samples')
parser$add_argument(
  '-z', '--zscore', default=4, type="integer",
  help='zscore cutoff used to flag outlier samples'
)
args <- parser$parse_args()

sampleQc(
  args$annotTsv, args$primaryID, args$stratify, args$prefix, args$bamQcTsv,
  args$vcfQcTsv, args$QcCutoffs, args$samplePc, args$refPc, args$zscore)
