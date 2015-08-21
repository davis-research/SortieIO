#' Fill A Plot In With Similar Values
#'
#' @param currentDf This is a data.frame with (x,y) columns representing your
#'   current points.
#' @param corners This is a vector set up as (minimum x, maximum x, minimum y,
#'   maximum y) representing the size plot you want to be filled.
#' @param speciesDf This is a data.frame with species, density, mean dbh and sd
#'   of dbh. DBH could represent any numerical value needed.
#'
#' @return This function should return a data.frame with Species, X, Y, and
#'   DBH that contains new records to fill in the rest of the plot space.


fillPlot <- function(currentDf, corners, speciesDf, retainCurrent=FALSE){

  ## get current Corners for later.
  currentCorners <- c(min(currentDf$X), max(currentDf$X), min(currentDf$Y), max(currentDf$Y))

  ## get area of corners
  area <- (corners[2]-corners[1])*(corners[4]-corners[3])

  ## calculate number of trees needed to fill the space
  speciesDf$counts <- round(speciesDf$density * area)

  ##make response table
  #print(sum(speciesDf$counts))
    response <- data.frame(X=NA, Y=NA,
                           Species=NA, Type=NA,
                           Diam=NA, Height=0,
                           Plot=NA, stringsAsFactors=FALSE)

    rowCounter <- 1

    ## for each unique species
    for(i in 1:nrow(speciesDf)){
      rowEnd <- NULL
      rowEnd <- rowCounter+speciesDf[i, "counts"]-1

      response[rowCounter:rowEnd, "Plot"] <- speciesDf[i, "Plot"]
      response[rowCounter:rowEnd, "Species"] <- speciesDf[i, "Species"]
      response[rowCounter:rowEnd, "X"] <- runif(speciesDf[i, "counts"],
                                                corners[1], corners[2])
      response[rowCounter:rowEnd, "Y"] <- runif(speciesDf[i, "counts"],
                                                corners[3], corners[4])
      response[rowCounter:rowEnd, "Diam"] <- rlnorm(speciesDf[i, "counts"],
                                                    speciesDf[i, "meandbh"],
                                                    speciesDf[i, "sddbh"])


      rowCounter <- rowCounter+speciesDf[i, "counts"]


    }

    response$Height <- 0
    response$Type <- "Adult"

    ## now remove any that fall within currentCorners
    removeme <- which(response$X > currentCorners[1] &
                           response$X < currentCorners[2] &
                        response$Y > currentCorners[3] &
                        response$Y < currentCorners[4])
    response <- response[-removeme,]

    if(retainCurrent==TRUE){
      response <- rbind(response, currentDf)
    }

    row.names(response) <- 1:nrow(response)
  return(response)
}

