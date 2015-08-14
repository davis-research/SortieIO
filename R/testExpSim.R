#' Test For Differences Between Actual and Simulated Data
#'
#' This function will perform one-sample t-tests, with a manually set mu, for
#' the data that you enter. Even if you want to set a manual mu value, you need
#' to include an experimental / expected Df.
#'
#' @param expDf An experimental data.frame with columns "Step", "Species", and a
#'   column named as the value inputted in "charactername" parameter.
#' @param simDf A data.frame with simulated values, with columns "Step",
#'   "Species", and a column named as the value inputted in "charactername"
#'   parameter.
#' @param charactername The name of the column of interest, e.g.,
#'   "AdultAbsoluteDensity"
#' @param mymu If mymu is set to something other than null, it will be used as
#'   the mu value for a one-sample t-test. If left blank, means from expDf will
#'   be used.
#' @param write If write is set to true, this function will write a CSV file
#'   with filename provided
#' @param filename If write is set to true, this function will write a CSV file
#'   with this filename.
#'
#' @return This function returns a data.frame with Step, Species, charactername,
#'   pval, and signif columns.
#'
#' @export

testExpSim <- function(expDf, simDf, charactername, mymu=NULL, write=F, filename=""){
  expDf$Species <- as.character(expDf$Species)
  simDf$Species <- as.character(simDf$Species)
  duplicatedSpecies <- c(unique(simDf$Species), unique(expDf$Species))
  duplicatedSpecies <- duplicatedSpecies[duplicated(duplicatedSpecies)]

  newSimDf <- data.frame()
  newExpDf <- data.frame()
  for(i in 1:length(duplicatedSpecies)){
    newSimDf <- rbind(newSimDf, simDf[simDf$Species==duplicatedSpecies[i],])
    newExpDf <- rbind(newExpDf, expDf[expDf$Species==duplicatedSpecies[i],])
  }

  simDf <- newSimDf
  expDf <- newExpDf
  ## convert back to factors
  expDf$Species <- as.factor(expDf$Species)
  simDf$Species <- as.factor(simDf$Species)

  ## get response df started
  responsedf <- aggregate(expDf[, charactername],
                          by=list(expDf$Step, expDf$Species),
                          FUN=mean,
                          na.rm=T)
  ## remove any NAs (instances where means could not be calculated)
  responsedf <- responsedf[!is.na(responsedf$x),]
  colnames(responsedf) <- c("Step", "Species", charactername)
  if(length(mymu) > 0){
    responsedf[,charactername] <- mymu
  }

  for(i in 1:nrow(responsedf)){
    relevantSimRow <- simDf[simDf$Step==responsedf[i, "Step"] &
                              simDf$Species==responsedf[i, "Species"],
                            charactername]
    if(length(relevantSimRow) > 5){
    responsedf[i, "simMean"] <- mean(relevantSimRow, na.rm=T)

    responsedf[i, "pval"] <- t.test(relevantSimRow,
                                    mu=responsedf[i, charactername])$p.value
    } else{
      responsedf[i, "simMean"] <- length(relevantSimRow)
      responsedf[i, "pval"] <- 1
    }
  }

  responsedf$signif <- ifelse(responsedf$pval > 0.05, "NS", "Sig")
  if(write==T){
    write.csv(responsedf, file=filename, row.names = F)
  }
  return(responsedf)
}


#store <- testExpSim(bbbExpDf, bbbSimDf, "AdultAbsBA", mymu=0)
#head(store)
