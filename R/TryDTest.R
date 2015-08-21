#' Tries a Dixon test for outliers.
#'
#' This function returns a 1 if there's an error in the Dixon test, otherwise,
#' it reports a p-value.
#' @param ... This function takes the parameters of a dixon test.
#'
#'@export
TryDTest <- function(...){
  require(outliers)
  obj <- try(dixon.test(...), silent=T)
  if (is(obj, "try-error")) return(1) else return(unname(obj$p.value))
}
