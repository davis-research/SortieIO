seedlings <- read.table("../inst/extdata/SmSeedlingData.txt", sep="\t", header=T, stringsAsFactors=F)
seedlings <- convertPlotNames(seedlings, "PLOT_NAME", T)
## clean up
seedlings <-seedlings[,c(17, 1:15)]
seedlings <- seedlings[, -3]
numSeedlings <- changeColsBySpp(seedlings, "COUNT")
finalSdlDf <- data.frame(plot=seedlings$PLOT_NAME,
                         subplot=seedlings$SUBPLOT,
                         sppcode=seedlings$SPPCODE,
                         maxht=seedlings$SIZE_CLASS,
                         year=numSeedlings[,1],
                         nSeedlings=numSeedlings[,2],
                         stringsAsFactors=F)
finalSdlDf <- finalSdlDf[!is.na(finalSdlDf$plot),]

finalSdlDf <- finalSdlDf[order(finalSdlDf$plot, finalSdlDf$subplot, finalSdlDf$sppcode, finalSdlDf$year, finalSdlDf$maxht),]
finalSdlDf <- finalSdlDf[-which(finalSdlDf$sppcode=="ABPS"),]
finalSdlDf <- reNumber(finalSdlDf)
ssdSeedlings <- finalSdlDf
save(ssdSeedlings, file="data/ssdSeedlings.RData")
