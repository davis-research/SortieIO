#' Get the difference between two numbers
#'
#' A simple function to calculate the difference between the maximum value and the minimum value of x.
#' 
#' @param x A vector of numbers.
#'
#' @return This function returns a number.
#'
#' @examples
#' getRange(c(12, 2,4))
#' @export

getRange <- function(x){
  if(!is.numeric(x)) stop("x must be numeric")
  return(max(x)-min(x))
  }
