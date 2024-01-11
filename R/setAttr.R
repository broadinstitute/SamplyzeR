#' Update a Sample Dataset
#'
#' @description
#' Update entries of a sample dataset, such as the df, bamQcMetr, vcfQcMetr.
#'
#' @param sds A sample dataset object
#' @param attributes character vector specifying the attributes to add or update in the sample Dataset
#' @param data data.frame, content to add to the dataframe
#' @param primaryID primary key of input data frame
#' @param overwrite logical, indicating whether to overwrite existing attributes if they already exist
#'
#' @return An updated sample dataset
#' @seealso{\link{setAttr.sampleDataset}}
#' @export

setAttr <- function (sds, ...) UseMethod('setAttr', sds)

#' Set attributes of a sample dataset
#'
#' @param sds sampleDataset
#' @param attributes character vector specifying the attributes to add or update in the sample Dataset
#' @param data data.frame, content to add to the dataframe
#' @param primaryID primary key of input data frame
#' @param overwrite logical, indicating whether to overwrite existing attributes if they already exist
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
