#' Update a Sample Data Set
#'
#' Update entries of sample data set, such as the df, bamQcMetr, vcfQcMetr
#'
#' @param sds sample dataset sds
#' @param ... other arguments
#'
#' @return sds
#' @export
#'
setAttr <- function (sds, ...) UseMethod('setAttr', sds)

#' Set attributes of a sample dataset
#'
#' @param sds sampleDataset
#' @param attributes which entry to add to sample Dataset
#' @param data data.frame, content to add to the dataframe
#' @param by primary key of input data frame
#'
#' @return updated sampleDataset sds
#' @export

setAttr.sampleDataset <- function(sds, attributes, data, primaryID,
                                  overwrite = F) {
  if (!is.data.frame((data)))
    stop("Input should be dataframe!")
  if (!any(data[[primaryID]] %in% sds$df[[sds$primaryID]])) {
    stop("There's no overlap between input and sds primary keys.
         please double check your input")
  }
  if (attributes %in% attributes(sds)$names) {
    if(overwrite) {
      toDrop = which(names(sds$df) %in% sds$annotations)
      sds$df = sds$df[-toDrop]
    } else {
      stop("Attribute already already exists, set overwrite = T to overwrite.")
    }
  }
  sds$df = merge(sds$df, data, by.x = sds$primaryID, by.y = primaryID,
                    all.x = T)
  sds[attributes] = list(names(data)[which(names(data) != primaryID)])
  return(sds)
}
