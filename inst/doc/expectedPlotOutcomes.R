### R code from vignette source 'expectedPlotOutcomes.Rnw'

###################################################
### code chunk number 1: expectedPlotOutcomes.Rnw:24-34
###################################################
library(MakeMyForests)
library(disperseR)
library(SortieIO)

head(ssdSeedlings)
head(expandedTrees)






###################################################
### code chunk number 2: expectedPlotOutcomes.Rnw:39-61
###################################################
## clean up
seedlings <- ssdSeedlings[which(ssdSeedlings$nSeedlings >0) ,]
seedlings <- aggregate(seedlings$nSeedlings, by=list(seedlings$plot,
                                                     seedlings$subplot,
                                                     seedlings$sppcode,
                                                     seedlings$year),
                       FUN=sum)
colnames(seedlings) <- c("plot", "subplot", "species", "measyear", "nSdl")


expandedSeedlings <- data.frame(species=seedlings$species,
                                plot=seedlings$plot, subplot=seedlings$subplot,
                                treeid=0, ingrowth=NA, firstrec=seedlings$measyear,
                                deathyear=NA, x=NA, y=NA,
                                measyear=seedlings$measyear, dbh=NA, stage="seedling",
                                basalarea=NA, mindbh=NA,
                                nSdl=seedlings$nSdl, stringsAsFactors=F)

## set up nSdl in expandedTrees so we can merge
expandedTrees$nSdl <- 1
head(expandedTrees)
expandedSeedlings <- rbind(expandedTrees, expandedSeedlings)


###################################################
### code chunk number 3: expectedPlotOutcomes.Rnw:67-137
###################################################
plotChars <- aggregate(expandedTrees$basalarea,
                       by=list(expandedTrees$plot,
                               expandedTrees$species,
                               expandedTrees$measyear,
                               expandedTrees$stage),
                       FUN=length)
colnames(plotChars) <- c("plot", "species", "year", "stage", "AbsDen")

## do the seedlings separately
plotSdls <- aggregate(expandedSeedlings$nSdl,
                      by=list(expandedSeedlings$plot,
                              expandedSeedlings$species,
                              expandedSeedlings$measyear),
                      FUN=sum)
colnames(plotSdls) <- c("plot", "species", "year", "nSdl")
test <- merge(plotSdls,
              plotChars[plotChars$stage=="seedling",c(1,2,3,5)],
              by=c("plot", "species", "year"),
              all=T)
test[is.na(test$nSdl), "nSdl"] <- 0
test[is.na(test$AbsDen), "AbsDen"] <- 0
test$sum <- test$nSdl + test$AbsDen
test$AbsDen <- test$sum
test <- test[,c(1:3,5)]
test$stage <- "seedling"


rawr <- merge(test, plotChars,
              by=c("plot", "species", "year", "stage"),
              all=T)
rawr[rawr$stage=="seedling", "AbsDen.y"] <- rawr[rawr$stage=="seedling", "AbsDen.x"]

plotChars <- rawr[, c(1:4,6)]
colnames(plotChars) <- c("plot", "species", "year", "stage", "AbsDen")

absBA <- aggregate(expandedTrees$basalarea,
                   by=list(expandedTrees$plot,
                           expandedTrees$species,
                           expandedTrees$measyear,
                           expandedTrees$stage),
                   FUN=sum)
colnames(absBA) <- c("plot", "species", "year", "stage", "AbsBA")

plotChars <- merge(plotChars, absBA,
                   by=c("plot", "species", "year", "stage"), all=T)
## put in plot normalizer
plotChars <- merge(plotChars, ssdPlotDesc[,c(1,3)])

plotChars$AbsDenByHa <- plotChars$AbsDen/plotChars$size_ha
plotChars$AbsBAByHa <- plotChars$AbsBA/plotChars$size_ha

plotChars$year <- as.numeric(plotChars$year)

## prepping for input into big table
## making years into steps
uniqueplots <- unique(plotChars$plot)
for(i in 1:length(uniqueplots)){
  plotChars[
    plotChars$plot==uniqueplots[i], "year"] <- plotChars[
      plotChars$plot==uniqueplots[i], "year"] -
    ssdPlotDesc[ssdPlotDesc$plot==uniqueplots[i], "minYear"]
   + 1
}

## make fake plot names into real plot names
##

plotChars <- convertPlotNames(plotChars, "plot", F)

plotChars <- plotChars[!is.na(plotChars$plot),]


###################################################
### code chunk number 4: expectedPlotOutcomes.Rnw:142-183
###################################################


plotCharUniques <- unique(plotChars[, c("plot", "species", "year")])
## set up end table

## get ready to fill with the variables that we have

## change colnames to match some
colnames(plotChars)[c(1:3,9 )] <- c("Species", "Step", "Stage", "Plot")

AdultAbsBA <- subset(plotChars, Stage=="tree",
                     select=c("Plot", "Step", "Species", "AbsBA"))
SaplAbsBA <- subset(plotChars, Stage=="sapling",
                    select=c("Plot", "Step", "Species", "AbsBA"))

AdultAbsDen <- subset(plotChars, Stage=="tree",
                      select=c("Plot", "Step", "Species", "AbsDen"))
SdlAbsDen <- subset(plotChars, Stage=="seedling",
                    select=c("Plot", "Step", "Species", "AbsDen"))
SaplAbsDen <- subset(plotChars, Stage=="sapling",
                     select=c("Plot", "Step", "Species", "AbsDen"))


newRealPlots <- data.frame(Plot=plotCharUniques[,1],
                           Step=plotCharUniques[,3],
                           Species=plotCharUniques[,2],
                           stringsAsFactors=F)

newRealPlots <- merge(newRealPlots, AdultAbsBA, all=T)
newRealPlots <- merge(newRealPlots, AdultAbsDen, all=T)
colnames(newRealPlots)[4:5] <- c("AdultAbsBA", "AdultAbsDen")

newRealPlots <- merge(newRealPlots, SaplAbsBA, all=T)
newRealPlots <- merge(newRealPlots, SaplAbsDen, all=T)
colnames(newRealPlots)[6:7] <- c("SaplAbsBA", "SaplAbsDen")

newRealPlots <- merge(newRealPlots, SdlAbsDen, all=T)
colnames(newRealPlots)[8] <- c("SdlAbsDen")

head(newRealPlots)
tail(newRealPlots)


