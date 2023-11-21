#' Save an sds to a specific format
#'
#' @description Save an sds to tsv, RDS or excel format
#'
#' @param sds sample dataset
#' @param ... Additional arguments to be passed.
#'
#' @export
#' @seealso{\link{sort.sampleDataset}}
#' @seealso{\link{save.sampleDataset}}
#' @seealso{\link{dim.sampleDataset}}
#' @seealso{\link{print.sampleDataset}}

save <-function(sds, ...) UseMethod('save')

#' Write sample dataset to tsv, RDS or excel files with S3 method save.
#'
#' @param sds sample dataset
#' @param tsv path and output name of tsv file
#' @param RDS path and output name of RDS file
#' @param xls path and output name of excel file
#'
#' @export

save.sampleDataset <- function(sds, RDS = NULL, tsv = NULL, xls = NULL) {
  if (is.null(RDS) & is.null(tsv) & is.null(xls)) {
    stop("Please specify at least one format (RDS, tsv or xls) for output")
  }
  if(!is.null(tsv)) {
    write.table(sds$df, file = tsv, sep = '\t', row.names = F, quote = F)
  }
  if (!is.null(RDS)) {
    saveRDS(sds, file = RDS)
  }
  if (!is.null(xls)) {
    WriteXLS::WriteXLS(sds$df, ExcelFileName = xls)
  }
}

#' Update index of sample dataset by different annotation categories
#'
#' @param sds sample dataset
#' @param by sort by which category
#' @return A sample dataset `sds` with updated index.
#'
#' @export

sort.sampleDataset <- function(sds, by) {
  byCol = which(names(sds$df) == by)
  sds$df = sds$df[order(sds$df[, byCol], na.last = T),]
  sds$df$index = 1:dim(sds$df)[1]
  return(sds)
}

#' Show dimensions of sample dataset sds
#'
#' @param sds Sample dataset `sds`.
#' @return a vector of rows and columns of the data frame of sample data set
#'
#' @export
dim.sampleDataset <- function(sds){
  dim(sds$df)
}

#' Print information about the sample dataset `sds`.
#'
#' @param sds Sample dataset `sds`.
#'
#' @export
print.sampleDataset <- function(sds) {
  cat("\nClass: SampleDataset\n",
      "Samples:   ", dim(sds)[1], "\n",
      "Attributes:", attributes(sds)$names, "\n\n"
      )
}
