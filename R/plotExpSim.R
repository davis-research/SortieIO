#' Plot Expected And Simulated Values
#'
#' This function will take two data.frames, a simulated and an expected data.frame, and plots the points over top of each other to visualize just how closely they match.
#'
#' @param expDf This is a data.frame of expected results for a plot. Must contain a "Step" column (x-axis), a "Species" column (color factor), and a value column that has the name of the "charactername" parameter.
#' @param simDf This is a data.frame of simulated results for a plot. Must contain a "Step" column (x-axis), a "Species" column (color factor), and a value column that has the name of the "charactername" parameter.
#' @param charactername The name of the column to plot on the y-axis.
#' @param means Currently set to false (default). If TRUE, function will plot means of simulated values instead of all values in simDf.
#' @param filename The filename to write this plot to. Include path if necessary.
#' @param exec If exec is TRUE, then the program will write the plot image to file.
#' @param exportSimDf If exportSimDf is TRUE, then the function will return a data.frame of the simDf. This is most often used when means=T, and the function returns the means of simDf.
#'
#' @return This function
#' @export

plotExpSim <- function(expDf, simDf, charactername, means=F, filename="", exec=F, exportSimDf=F){

  if(!("Step" %in% colnames(expDf)) |
     !("Step" %in% colnames(simDf)) |
     !("Species" %in% colnames(expDf)) |
     !("Species" %in% colnames(simDf)) |
     !(charactername %in% colnames(expDf)) |
     !(charactername %in% colnames(simDf))){
    stop("Sorry, your columns are not formatted correctly.")
  }

  ## if mean is true, then aggregate to get the mean and plot simulated values that way.
  ##
  if(means==T){
    simMeansDf <- aggregate(simDf[, charactername],
                          by=list(simDf$Species, simDf$Step),
                          FUN=mean, na.rm=T)
    colnames(simMeansDf) <- c("Species", "Step", charactername)
    simDf <- simMeansDf
  }

  expDf$Species <- as.factor(expDf$Species)
  simDf$Species <- as.factor(simDf$Species)

  ymax <- ifelse(max(expDf[, charactername], na.rm=T) > max(simDf[, charactername], na.rm=T),
                 max(expDf[, charactername], na.rm=T), max(simDf[, charactername], na.rm=T))
  xmax <- max(expDf$Step)
  if(exec==T){
    pdf(file=filename)
  }
  plot(simDf$Step,
       simDf[,charactername],
       col=simDf$Species,
       xlim=c(1,xmax),
       ylim=c(0, ymax),
       xlab="Step (Yr)",
       ylab=charactername,
       pch=3)
  points(expDf$Step,
         expDf[,charactername],
         col=expDf$Species,
         pch=15)
  if(exec==T){
    dev.off()
  }

if(exportSimDf==T){
  return(simDf)
}

}

