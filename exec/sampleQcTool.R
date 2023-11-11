#!/usr/bin/env Rscript
if (!require("samplyzer"))devtools::install_github("xiaolicbs/samplyzer", ref = "fix", force = TRUE)
library(argparse)
library(samplyzer)

# Function to create sampleDataset
createSds <- function(bamQcInput, vcfQcInput, annotTsv, stratify, primaryID) {
  stratify <- unlist(strsplit(stratify, ','))
  sds <- sampleDataset(annotations, primaryID, bamQcMetr, vcfQcMetr, stratify)
}

qcMetrScatterPlots <- function(sds, stratify_values, outliers, prefix, geom) {
  # splice stratify_values
  stratify_list <- strsplit(stratify_values, ',')[[1]]
  for (stratify in stratify_list) {
    for (metric in sds$qcMetrics) {
      tryCatch({
        # qcMetric and stratify
        grobList <- sampleQcPlot(sds, qcMetrics = metric, annot = stratify,
                                 geom = geom, outliers = outliers, show = T)
        filename <- paste(prefix, stratify, metric, 'QcMetrScatter.pdf', sep = '_')
        # save
        ggplot2::ggsave(filename, grobList, width = 12, height = 15)
      }, error = function(e) {
        cat("Error occurred for", stratify, metric, ": ", conditionMessage(e), "\n")
        # next
      })
    }
  }
  cat("qcMetr Plots\n")
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
    cutoffs = cutoffTsv
  }
  return(cutoffs)
}

stratfityAnalysis <- function(sds, stratify, cutoffDf, prefix) {
  cat(" o Perform a stratified QC metrics analysis\n")
  # Calculate z-scores for each batch across different ancestry
  stratify_list <- strsplit(stratify, ',')[[1]]
  sds = calZscore(sds,
                  strat=c(stratify_list),
                  qcMetrics = sds$vcfQcMetr)
  cat(" o Filter samples with hard cutoffs and z-scores\n")
  sds = flagSamples(sds, cutoffs=cutoffDf, zscore=4)
  save(sds, RDS = paste(prefix, 'RDS', sep = '.'),tsv = paste(prefix, 'tsv', sep = '.'))
  return(TRUE)
}

pcaAnalysis <- function(sds, samplepc, refpc, primaryID, outliers, stratify_values, qcX, qcY, prefix) {
  if (!all(is.null(refpc), is.null(samplepc))) {
    cat(" o Add genotype PCs to sample Dataset\n")
    sds = setAttr(sds, attributes = 'PC', data = samplepc,
                  primaryID = primaryID)

    cat(" o Infer ancestry from PCA results\n")
    sds = inferAncestry(sds,
                        trainSet = refpc[, grep("^PC", names(refpc))],
                        knownAncestry = refpc$group)
    stratify_list <- strsplit(stratify_values, ',')[[1]]
    for (stratify in stratify_list) {
      if(!all(is.null(qcX), is.null(qcY))){
        tryCatch({
          qcCorr = samplyzer:::scatter(
            data = sds$df, x = qcX, y = qcY, strat = stratify,
            outliers = outliers, primaryID = sds$primaryID)
          # save
          filename <- paste(prefix, stratify, qcX, qcY, 'QcMetrPcaAnalysis.pdf', sep = '_')
          ggplot2::ggsave(filename, qcCorr, width = 12, height = 15)
        }, error = function(e) {
          cat("Error occurred for", stratify, ": ", conditionMessage(e), "\n")
        })
      }
    }
  }
  cat("PCA Analysis\n")
  return(TRUE)
}

#  main
toolDescr <- paste('This tool performs routine sample QC process, including sample QC plots, PCA plots, flag outliers based on certain cutoffs.')
args <- commandArgs(trailingOnly = TRUE)
parser <- ArgumentParser(description = toolDescr)
# Add command-line arguments
parser$add_argument("-b", "--bamQcMetr", required=TRUE, help = "Path to BAM QC metrics file")
parser$add_argument("-v", "--vcfQcMetr", required=TRUE, help = "Path to VCF QC metrics file")
parser$add_argument("-a", "--annotations", required=TRUE, help = "Path to sample annotations file")
parser$add_argument("-d", "--primaryID", required=TRUE, help = "Name of primary IDs in input TSVs to specify each sample")
parser$add_argument(
  "--stratify",required=TRUE, help = "List of attributes for stratification")

# optional arguments
parser$add_argument(
  "-o", "--outliers", default = 'Sample-001', help = "Sample ID")
parser$add_argument(
  "--cutoffTsv", default = NULL,
  help = 'a tsv file that contains hard cutoffs for QC metrics')
parser$add_argument(
  "-g", "--samplepc", default = NULL,
  help = 'A tsv file with genotype PCs for each sample')
parser$add_argument(
  "-r", "--refpc", default = NULL,
  help = 'A tsv file with genotype PCs from a reference set of samples')
parser$add_argument(
  '-z', '--zscore', default=4, type="integer",
  help='zscore cutoff used to flag outlier samples')
parser$add_argument(
  "-p", "--prefix", default = 'examples', help = "Prefix of output files")
parser$add_argument(
  "-x", "--pcX", default = 'Mean_Coverage', help = "pca analysis QC metrics")
parser$add_argument(
  "-y", "--pcY", default = 'nHets', help = "pca analysis QC metrics")
parser$add_argument(
  "--geom", default ='scatter', help = "QC metrics geom")

# Parse command-line arguments
args <- parser$parse_args()



# Extract file paths and other parameters from parsed arguments
# Read data files
bamQcMetr <- read.csv(args$bamQcMet, sep = '\t')
vcfQcMetr <- read.csv(args$vcfQcMetr, sep = '\t')
annotations <- read.csv(args$annotations, sep = '\t')
if (!is.null(args$cutoffTsv) && file.exists(args$cutoffTsv)) {
  cutoffTsv <- read.csv(args$cutoffTsv, sep = '\t')
}else{
  cutoffTsv <- NULL
}

sds <- createSds(bamQcMetr, vcfQcMetr, annotations, args$stratify, args$primaryID)
# qcMetr plot
qcMetrScatterPlots(sds, args$stratify, args$outliers, args$prefix, args$geom)
# stratfityAnalysis
myCutoffs <- cutoffDf(cutoffTsv)
stratfityAnalysis(sds, args$stratify, myCutoffs, args$prefix)
# pca
if(!is.null(args$refpc) && file.exists(args$refpc) && !is.null(args$samplepc) && file.exists(args$samplepc)){
  samplepc <- read.csv(args$samplepc, sep = '\t')
  refpc <- read.csv(args$refpc, sep = '\t')
  pcaAnalysis(sds, samplepc, refpc, args$primaryID, args$outliers, args$stratify, args$pcX, args$pcY, args$prefix)
}
