#' Process a plot's test data
#'
#' @export

processTestPlot <- function(plotname, sdir, yearoffset, charactername="", writeimage=NULL, writefile=NULL, byspecies=F){

  if(byspecies==T){
    myExpDf <- realPlots
  } else{
    myExpDf <- realPlots[realPlots$Plot==plotname,]
  }
  mySimDf <- batchOutFiles(plotname, sdir, yearoffset)
  myExpDf$Species <- as.factor(myExpDf$Species)

  ## make the plot
  if(length(writeimage) > 0){
    plotExpSim(myExpDf, mySimDf, charactername, exec=T, filename=writeimage)
  } else{
    plotExpSim(myExpDf, mySimDf, charactername, exec=F)
  }

  ## run the t.test
  if(length(writefile) > 0){
    store <- testExpSim(myExpDf, mySimDf, charactername, write=T, filename=writefile)
  } else{
    store <- testExpSim(myExpDf, mySimDf, charactername, write=F)
  }
  return(store)
}
