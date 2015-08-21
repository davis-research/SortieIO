#' Tries a t-test, returns 1 if there is an error
#'
#' Function taken from https://stat.ethz.ch/pipermail/r-help/2008-February/154167.html
#'
#' This function tries a t-test and returns 1 if there's an error, instead of stopping execution.
#'
#' @param ... This function takes all the parameters that a t-test does.
#'
#' @return This function returns a number representative of the p-value.
#' @export

TryTTest <- function(...) {
  obj<-try(t.test(...), silent=TRUE)
  if (is(obj, "try-error")) return(1) else return(obj$p.value)
}


