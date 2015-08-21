#' Renumber rows in a data.frame to deal with missing rows.
#'
#' This function cleans up the rownames of a data.frame by renumbering from 1 to the total number of rows in the data.frame.
#' @param x The target data.frame
#' @return This function returns a data.frame
#' @examples
#' reNumber(data.frame(x=1:4, y=5:8)[-1,])
#' @export

reNumber <- function(x){
  if(!is.data.frame(x)){ stop("x should be a data.frame")}
  rownames(x) <- 1:nrow(x)
  return(x)
}
