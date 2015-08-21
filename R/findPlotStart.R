#' Find The Starting Place For A Map Centered In A Larger Map
#'
#' This function finds the place where a smaller range of (x,y) should begin in
#' order to be centered in a larger (x,y) space.
#'
#' @param newmapx The range of x in your larger, new map.
#' @param newmapy The range of y in your larger, new map.
#' @param plotx The range of x in your smaller map.
#' @param ploty The range of y in your smaller map.
#'
#' @examples
#' findPlotStart(300, 300, 100, 150)


findPlotStart <- function(newmapx, newmapy, plotx, ploty){
  startingx <- (newmapx-plotx) / 2
  startingy <- (newmapy-ploty) / 2
  return(c(startingx, startingy))
}
