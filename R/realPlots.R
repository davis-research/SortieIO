#' Real Plot Data (realPlots)
#'
#' A data.frame containing 343 observations of 13 variables. These observations are summaries of real measurements of plots taken between 1982-2013.
#'
#' @format A data.frame with 343 rows and 10 variables
#' \describe{
#' \item{Plot}{The plot name}
#' \item{Step}{The measurement year}
#' \item{Species}{The species shortcode, see MakeMyForests or disperseR for more information}
#' \item{AdultAbsDen}{The total number of trees in an area (hectare)}
#' \item{AdultAbsBA}{The total basal area of trees in an area (hectare)}
#' \item{TotalTrees}{The total trees in the area (all species)}
#' \item{TotalBA}{The total basal area in the area (all species)}
#' \item{AdultRelDen}{The relative density, as a value from 0-1, of a particular tree species}
#' \item{AdultRelBA}{The relative basal area, as a value from 0-1, of a particular tree species}
#' \item{SdAbsDen}{The total number of seeds for a particular species in the area}
#' \item{TotalSeeds}{The total number of seeds in the entire plot, all species}
#' \item{SdRelDen}{The relative density, from 0-1 of the seeds of a particular species in the plot.}
#' }
#'
"realPlots"
