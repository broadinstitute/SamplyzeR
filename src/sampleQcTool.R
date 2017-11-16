#!/usr/bin/env Rscript
library(devtools)
install_github('xiaolicbs/SamplyzeR')
library(optparse)
library(SamplyzeR)

.stratQcMetrHail <- function() {
  return(c("nDeletion", "nInsertion", "nSNP", "rTiTv", "rHetHomVar", "rInsertionDeletion"))
}

.stratQcMetrGATK <- function() {
  return(c("nEvalVariants", "nInsertions", "nDeletions", "TiTvRatio", "InsDelRatio", "HetHomeRatio"))
}
option_list <- list(
  make_option(c("-b", "--bamQcMetr"), help = "A tsv for BAM Level quality control metrics", default = NULL),
  make_option(c("-v", "--vcfQcMetr"), help = "A tsv for VCF Level quality control metrics", default = NULL),
  make_option(c("-a", "--annotations", help = "A tsv for sample annotation file")),
  make_option(c("-d", "--primaryID"), help = "primary IDs used to specific each sample"),
  make_option(c("-s", "--stratify"), help = "A list of attributes used in the stratified QC metrics analysis, separated by comma"),
  make_option(c("-g", "--samplepc"), help = 'A tsv file with genotype PCs for each sample', default = NULL),
  make_option(c("-r", "--refpc"), help = 'A tsv file with genotype PCs from a reference set of samples', default = NULL),
  make_option(c("-p", "--prefix", help = "Prefix of output files")),
  make_option(c("--cutoffs", help = 'a tsv file that contains hard cutoffs for QC metrics', default = NULL)),
  make_option(c("-i", "--inbreedfile"), help = "Heterozygosity test results: first column shold 
              be sample Id, second column should Fstat",default= NULL),
  make_option(c("-ibd", "--ibdfile", help = "A tsv for Relatedness test,first tow columns are sample ID"), default = NULL),
  make_option(c("-pihat", "--PIHAT"), help = "The column name of PI_HAT value , for example : ibd.PI_HAT", default=NULL),
  make_option(c("-pbam", "--plotbamqc", help = "TRUE means ploting bam QC figure")),
  make_option(c("-pvcf", "--plotvcfqc", help = "TRUE means ploting vcf QC figure")),
  make_option(c("-order", "--sampleorder"), help = "A tsv for sample order index, first column is sample id, second is order number", default = NULL),
  make_option(c("-z", "--zscoreqc"), help = "A list of QC attributes used to calculate zscore, separated by comma", default= NULL)Sa
)

opt <- parse_args(OptionParser(option_list=option_list))

if(!is.null(opt$bamQcMetr)) {
  bamQcMetr = read.csv(opt$bamQcMetr, sep = '\t')
} else {
  bamQcMetr = NULL
}
if(!is.null(opt$vcfQcMetr)) {
  vcfQcMetr = read.csv(opt$vcfQcMetr, sep = '\t')
  if(!is.null(opt$inbreedfile)) {
    inbreed <- read.delim(file=opt$inbreedfile,sep="\t",header=T)
    vcfQcMetr <- merge(vcfQcMetr,inbreed[,1:2],by.x=1,by.y=1,all.x=T)
  }
} else {
  vcfQcMetr = NULL
}
annotations = read.csv(opt$annotations, sep = '\t')

stratify = unlist(strsplit(opt$stratify, ','))

if (!all(stratify %in% names(annotations))) {
  stop("Not all stratified factors are in the annotation file, please double check your input stratify option.")
}


## Construct an Sample Dataset object
## A Sample Data Set object contains several attributes, including a data frame, which contains all the data for samples and
## several other attributes that contains metadata, such as qcMetrics, bamQcMetr, vcfQcMetr and PrimaryID. To check attributes of your current SDS
sds = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                    annotations = annotations, primaryID = opt$primaryID)

if(!is.null(opt$sampleorder)) {
  ordersample = read.csv(opt$sampleorder, sep = '\t')
  tsort= FALSE
  sds$df <- merge(sds$df,ordersample,by.x=1,by.=1) 
} else {
  tsort= TRUE
  sds$df$index <- 1:(dim(sds$df)[1])
}

## Perform BAM sample QC
# flag samples with cutoffs
if (is.null(opt$cutoff)) {
  cutoffs = data.frame(
    qcMetrics = c('PctLt30bp', 'ContaminationEstimation', 'PctChimericReads','callRate', 'Fstat'),
    value = c(0.01, 0.02, 0.05,0.98,-0.05),
    greater = c(T, T, T,F,F),
    stringsAsFactors = F
  )
} else {
   cutoffs = read.csv(opt$cutoffs, sep = '\t')
}
#sds = flagSamples(sds, cutoffs = cutoffs)
# Save output to tables

if(opt$plotbamqc) {
cat("Save BAM QC plot to pdf.\n")
  for (i in 1:length(sds$annotation)) {
    grobList = sampleQcPlot(sds, qcMetrics = sds$bamQcMetr, annotation = sds$annotation[i],
                            geom = 'scatter', ncols = 2,show = F,sort= tsort)
    ggplot2::ggsave(paste(opt$prefix, sds$annotation[i], '.bam.QcPlot.pdf', sep = '.'), grobList, width = 15, height = 10)
    cat("Done.")
  }
}

if(opt$plotvcfqc) {
  cat("Save BAM QC plot to pdf.\n")
  for (i in 1:length(sds$annotation)) {
    grobList = sampleQcPlot(sds, qcMetrics = sds$vcfQcMetr, annotation = sds$annotation[i],
                            geom = 'scatter', ncols = 2,show = F,sort= tsort)
    ggplot2::ggsave(paste(opt$prefix, sds$annotation[i], '.vcf.QcPlot.pdf', sep = '.'), grobList, width = 15, height = 10)
    cat("Done.")
  }
}


if (!all(is.null(opt$refpc), is.null(opt$samplepc))) {
  cat(" o Load Sample and Reference genotype PCs\n")
  samplepc = read.csv(opt$samplepc, sep = '\t')
  refpc = read.csv(opt$refpc, sep = '\t')

  cat(" o Add genotype PCs to sample Dataset\n")
  sds = setAttr(sds, attributes = 'PC', data = samplepc, primaryID = opt$primaryID)

  cat(" o Infer ancestry from PCA results\n")
  sds = inferAncestry(sds, trainSet = refpc[,c('PC1', 'PC2', 'PC3')], knownAncestry = refpc$knownAncestry)
  sds = calZscore(sds, strat=c(stratify,'inferredAncestry'), qcMetrics = opt$zscoreqc)
} else {
  
  cat(" o Perform a stratified QC metrics analysis\n")
  ## Calculate z-scores for each batch across different ancestry
  sds = calZscore(sds, strat=stratify, qcMetrics = opt$zscoreqc)

}

  cat(" o Filter samples with hard cutoffs and z-scores\n")
  sds = flagSamples(sds, cutoffs = cutoffs, zscore = 4)

  if(!is.null(opt$ibdfile)) {
    ibd <- read.delim(file=opt$ibdfile,sep="\t",header=T)
    t <- which(ibd[,opt$PIHAT] >= 0.185)
    if(length(t) > 1) {
      ibd <- ibd[t,]
      fibd <- processibd(ibd,opt$PIHAT)
      sds$df <- merge(sds$df,fibd,by.x=1,by.y=1,all.x=T)
    }
  }  

cat("\nSave output to RDS and write out sample info.\n")
save(sds, RDS = paste(opt$prefix, 'RDS', sep = '.'),
     tsv = paste(opt$prefix, 'tsv', sep = '.'),
     xls = paste(opt$prefix, 'xls', sep = '.'))

cat(" o Done.")
