#'Process a plot's test data
#'
#'@param plotname The name of the plot. This must be the same as what is looked
#'  up by realPlot, or it will bounce back.
#'@param sdir The directory location of the output files
#'@param yearoffset If there's a run-up -- early data you want to throw out --
#'  add it here. Default is 0.
#'@param charactername The character name to analyze. Current options are
#'  "SdlAbsDen", "SaplAbsDen", "AdultAbsDen", "SaplAbsBA", and "AdultAbsBA"
#'@param writeimage Pass a filename in to allow the program to generate a plot
#'  and write it to file.
#'@param writefile Pass a filename in to allow the program to generate a CSV and
#'  write it to file.
#'@param byspecies If you want to separate the data by plot, set this to FALSE.
#'@param numsubplots The number of subplots in the file. It defaults to 1, the
#'  entire plot "subplot"
#'@param subplotid The subplot you want to analyze. Default ID is 0, for the
#'  entire plot.
#'@param testSim A testing function, if set to true, the execution will stop and
#'  return the simulated data instead of the full result.
#'@param combineAdults Defaults to true, if false, Adult analyses will exclude
#'  sapling and seedling counts and basal areas.
#'
#'@export

processTestPlot <- function(plotname, sdir, yearoffset, charactername="",
                            writeimage=NULL, writefile=NULL, byspecies=F,
                            numsubplots=1, subplotid=0, testSim=F, combineAdults=T){

  if(combineAdults==T){
    realPlots$AdultAbsBA <- realPlots$AdultAbsBA + realPlots$SaplAbsBA
    realPlots$AdultAbsDen <- realPlots$AdultAbsDen + realPlots$SaplAbsDen + realPlots$SdlAbsDen
  }

  if(byspecies==T){
    myExpDf <- realPlots
  } else{
    myExpDf <- realPlots[realPlots$Plot==plotname,]
  }
  mySimDf <- batchOutFiles(plotname, sdir, yearoffset, numsubplots)
  if(testSim==TRUE){
    return(mySimDf)
  }
  myExpDf$Species <- as.factor(myExpDf$Species)

  ## make the plot
  if(length(writeimage) > 0){
    plotExpSim(myExpDf, mySimDf, charactername, exec=T, filename=writeimage, subplotid=subplotid)
  } else{
    plotExpSim(myExpDf, mySimDf, charactername, exec=F, subplotid=subplotid)
  }

  ## run the t.test
  if(length(writefile) > 0){
    store <- testExpSim(myExpDf, mySimDf, charactername, write=T, filename=writefile, subplotid=subplotid, simTest=F)
  } else{
    store <- testExpSim(myExpDf, mySimDf, charactername, write=F, subplotid=subplotid, simTest=F)
  }
  return(store)
}
