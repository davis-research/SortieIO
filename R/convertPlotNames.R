#' Convert plot names
#'
#' @export
convertPlotNames <- function(x, colname="", realnames=F){
  plotconversion <- data.frame(plotname=c("BBBPIPO", "FRPIJE", "EMRIDGE",
                                          "SUPILA", "POFLABMA", "PGABMA",
                                          "SFTRABMA", "WTABMA", "EMSLOPE",
                                          "SUABCO", "CCRPIPO", "CRCRPIPO"),
                               key=c("crackers", "octane", "bellow",
                                     "rigid", "distress", "sodium",
                                     "gravy", "chestnut", "reclusive",
                                     "trigger", "trinity", "realtor"), stringsAsFactors = F)
  if(realnames==F){
    colnames(plotconversion)[2] <- colname
  } else{
    colnames(plotconversion)[1] <- colname
  }
  tempdf <- merge(x, plotconversion, by=colname, all=T)
  tempdf[, colname] <- list(NULL)
  if(realnames==F){
    tempcols <- colnames(tempdf)
    tempcols[tempcols=="plotname"] <- colname
    colnames(tempdf) <- tempcols
  }else{
    tempcols <- colnames(tempdf)
    tempcols[tempcols=="key"] <- colname
    colnames(tempdf) <- tempcols
  }
  tempdf <- tempdf[!is.na(tempdf[,1]),]
  rownames(tempdf) <- 1:nrow(tempdf)
  return(tempdf)
}
