#' Process a plot's test data
#'
#' @export

processTestPlot <- function(plotname, sdir, yearoffset, charactername="",
                            writeimage=NULL, writefile=NULL, byspecies=F,
                            numsubplots=1, subplotid=0){

  if(byspecies==T){
    myExpDf <- realPlots
  } else{
    myExpDf <- realPlots[realPlots$Plot==plotname,]
  }
  mySimDf <- batchOutFiles(plotname, sdir, yearoffset, numsubplots)
  myExpDf$Species <- as.factor(myExpDf$Species)

  ## make the plot
  if(length(writeimage) > 0){
    plotExpSim(myExpDf, mySimDf, charactername, exec=T, filename=writeimage, subplotid=subplotid)
  } else{
    plotExpSim(myExpDf, mySimDf, charactername, exec=F, subplotid=subplotid)
  }

  ## run the t.test
  if(length(writefile) > 0){
    store <- testExpSim(myExpDf, mySimDf, charactername, write=T, filename=writefile, subplotid=subplotid)
  } else{
    store <- testExpSim(myExpDf, mySimDf, charactername, write=F, subplotid=subplotid)
  }
  return(store)
}
