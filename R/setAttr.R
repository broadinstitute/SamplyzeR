#' Update a Sample Data Set
#'
#' Update entries of sample data set, such as the df, bamQcMetr, vcfQcMetr
#'
#' @param object sample dataset object
#' @param ... other arguments
#'
#' @return object
#' @export
#'
setAttr <- function (object, ...) UseMethod('setAttr', object)

#' Set attributes of a sample dataset
#'
#' @param object sampleDataset
#' @param attributes which entry to add to sample Dataset
#' @param data data.frame, content to add to the dataframe
#' @param by primary key of input data frame
#'
#' @return updated sampleDataset object
#' @export

setAttr.sampleDataset <- function(object, attributes, data, primaryID,
                                  overwrite = F) {
  if (!is.data.frame((data)))
    stop("Input should be dataframe!")
  if (!any(data[[primaryID]] %in% object$df[[object$primaryID]])) {
    stop("There's no overlap between input and object primary keys.
         please double check your input")
  }
  if (attributes %in% attributes(sds)$names) {
    if(overwrite) {
      toDrop = which(names(sds$df) %in% sds$annotations)
      sds$df = sds$df[-toDrop]
      } else {
      stop( "Attribute already already exists, set overwrite = T to overwrite.")
    }
  }
  object$df = merge(object$df, data, by.x = object$primaryID, by.y = primaryID,
                    all.x = T)
  object[attributes] = list(names(data)[which(names(data) != primaryID)])
  return(object)
}
