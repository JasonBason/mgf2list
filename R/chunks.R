#' chunks function
#'
#' Extract a chunk of data from mgf data
#' What is returned: dataframe x is returned from ranges that are set
#'  in another dataframe: range
#' @param x data.frame to be chunked
#' @param ranges vector with row numbers defining beginnings and ends of data chunks
#' @export
chunks <- function(x, range){
  return(x[range[1]:range[2],])
}
