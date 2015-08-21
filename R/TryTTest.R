#' Tries a t-test, returns 1 if there is an error
#'
#' Function taken from https://stat.ethz.ch/pipermail/r-help/2008-February/154167.html
#' @export

TryTTest <- function(...) {
  obj<-try(t.test(...), silent=TRUE)
  if (is(obj, "try-error")) return(1) else return(obj$p.value)
}

TryDTest <- function(...){
  obj <- try(dixon.test(...), silent=T)
  if (is(obj, "try-error")) return(1) else return(obj$p.value)
}
