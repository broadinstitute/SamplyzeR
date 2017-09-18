#!/usr/bin/env Rscript

library(devtools)
install_github('xiaolicbs/genotype_qc')
library(optparse)
library(genotypeqc)

option_list <- list(
  make_option(c("-b", "--bamQcMetr"), help = "A tsv for BAM Level quality control metrics"),
  make_option(c("-v", "--vcfQcMetr"), help = "A tsv for VCF Level quality control metrics"),
  make_option(c("-a", "--attributes", help = "A tsv for attributes file for samples being quality controlled")),
  make_option(c("-d", "--primaryID"), help = "primary IDs used to specific each sample"),
  make_option(c("-s", "--stratify"), help = "A list of attributes used in the stratified QC metrics analysis, separated by comma"),
  make_option(c("-o", "--outputDir", help = "Output directory")),
  make_option(c("-p", "--prefix", help = "Prefix of output files"))
)
opt <- parse_args(OptionParser(option_list=option_list))

# construct a sample data set
opt = list(bamQcMetr = 'examples/bamQcMetr.tsv', vcfQcMetr = 'examples/vcfQcMetr.tsv',
           attributes = 'examples/sampleAttributes.tsv', stratify = 'ANCESTRY',
           primaryID = 'ExternalID', outputDir = '.'
)

bamQcMetr = read.csv(opt$bamQcMetr, sep = '\t')
vcfQcMetr = read.csv(opt$vcfQcMetr, sep = '\t')
attributes = read.csv(opt$attributes, sep = '\t')
stratify = unlist(strsplit(',', opt$stratify))

sds = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                    attributes = attributes, primaryID = opt$primaryID)
# calculate z-scores
sds = calZscore(sds, strat= opt$stratify)
sds = calZscore(sds)

# flag samples with cutoffs

## output
setwd(opt$outputDir)

# output to RDS database
save(sds, file = 'example.RDS')

# output pdf plots stratifying across different factors

# output summary statistics into an excel sheet
write.table('') # flagged, reasons

# PCA scatter plots

# correlation with different PCs
