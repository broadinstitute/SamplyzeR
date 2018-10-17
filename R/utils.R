.is.sds <- function(x) 'sampleDataset' %in% class(x)


loadInput <- function (input) {
  if (is.data.frame(input)) {
    df = input
  } else if (is.character(input)) {
    df = read.csv(input, sep='\t')
  } else {
    stop("Input is not a tsv file or a data.frame")
  }
  return(df)
}
