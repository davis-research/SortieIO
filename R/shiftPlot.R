#' Shift Plot Linearly By X And Y
#'
#' This function takes your old origin, your new origin, and your data.frame, and shifts to match the new origin.
#'
#' @param oldx Your old origin's x point (e.g., 0)
#' @param oldy Your old origin's y point
#' @param newx Your new origin x.
#' @param newy Your new oriign y.
#' @param x A vector of x-values to be transformed.
#' @param y A vector of y-values to be transformed.
#'
#' @examples
#' shiftPlot(0,0,100,100,c(2,4,5), c(4,5,6))

shiftPlot <- function(oldx, oldy, newx, newy, x, y){
  shiftx <- newx-oldx
  shifty <- newy-oldy
  x <- x + shiftx
  y <- y + shifty
  return(data.frame(x=x, y=y))
}
