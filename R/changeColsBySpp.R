#' Extract Column Info And Rearrage A Data.Frame
#'
#' This function takes columns that are names like (DensityABCO, DensityABMA,
#' DensityCADE) and changes the data.frame into something like (Species,
#' Density), with a key/value system instead. Check out the examples for how it
#' works. It's used within \code{\link{parseOutFile}}.
#'
#' @param df The data.frame to search
#' @param removalkey The repeated part of the column name to remove, e.g.,
#'   "Density"
#' @param dead If dead=T, append "Natural.Dead." to the removal key and subset
#'   that way.
#' @examples
#' testdf <- data.frame(DensityABCO=c(1,2), DensityABMA=c(3,4), DensityCADE=c(5,6), stringsAsFactors=F)
#' changeColsBySpp(testdf, "Density", dead=F)
#'
#' @export

changeColsBySpp <- function(df, removalkey, dead=F){

  ## if dead, append "Natural.Dead. to everything and subset
  if(dead==T){
    removalkey <- paste("Natural.Dead.", removalkey, sep="")
    temp <- df[, grep(removalkey, colnames(df), value=F, fixed=T)]
    ## else remove any NaturalDead and subset
  } else{
    if(length(grep("Natural.Dead", colnames(df), fixed=T))>0){
      temp <- df[, -grep("Natural.Dead", colnames(df), fixed=T)]
      temp <- temp[, grep(removalkey, colnames(temp), value=F, fixed=T)]
    } else{
      temp <- df[, grep(removalkey, colnames(df), value=F, fixed=T)]
    }


  }
  ## fix column names by removing the repeat stuff
  colnames(temp) <- gsub(removalkey, "", colnames(temp), fixed=T)
  ## init a newdf for response
  newdf <- data.frame()
  ## get our loop counter
  tempcols <- colnames(temp)

  ## for each column (species)
  for(i in 1:length(tempcols)){
    ## rbind the response df (newdf) to a new species set.
    newdf <- rbind(newdf,
                   data.frame(key=tempcols[i],
                              val=temp[,i],
                              stringsAsFactors=F)
                   )
  }

  return(newdf)
}



