#' Write sample dataset to tsv, RDS or excel files with S3 method save.
#'
#' @param object sample dataset
#' @param tsv path and output name of tsv file
#' @param RDS path and output name of RDS file
#' @param xls path and output name of excel file
#' @export

save.sampleDataset <- function(object, file = NULL, format = 'RDS') {
  # output options
  if (is.null(file)) stop("Please specify output file name.")
  if (!(format %in% c('tsv', 'xls', 'RDS'))) {
    stop("")
  }
  if(!is.null(tsv)) {
    write.table(object$df, file = tsv, sep = '\t', row.names = F, quote = F)
  }
  if (!is.null(RDS)) {
    saveRDS(object, file = RDS)
  }
  if (!is.null(xls)) {
    library(WriteXLS)
    WriteXLS(object$df, ExcelFileName = xls)
  }
}

#' Update index of sample dataset by different annotation categories
#'
#' @param object sample dataset
#' @param by sort by which category
#' @return an sample dataset object with updated index
#' @export

sort.sampleDataset <- function(object, by) {
  byCol = which(names(object$df) == by)
  object$df$index = order(order(object$df[ ,byCol], na.last = T))
  object
}

#' Show dimensions of sample dataset object
#'
#' @param object sample data set
#'
#' @return a vector of rows and columns of the data frame of sample data set
#' @export

dim.sampleDataset <- function(object){
  dim(object$df)
}
