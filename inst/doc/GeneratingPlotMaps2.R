### R code from vignette source 'GeneratingPlotMaps2.Rnw'

###################################################
### code chunk number 1: GeneratingPlotMaps2.Rnw:24-27
###################################################
library(MakeMyForests)
library(SortieTreeMaps)
load("../data/rotPlots.RData")


###################################################
### code chunk number 2: GeneratingPlotMaps2.Rnw:32-41
###################################################

plotnames <- unique(rotPlots$plot)

getRange <- function(x){max(x)-min(x)}
rangex <- aggregate(rotPlots$x, by=list(rotPlots$plot), FUN=getRange)
rangey <- aggregate(rotPlots$y, by=list(rotPlots$plot), FUN=getRange)

plotSizes <- data.frame(plot=rangex[,1], x=rangex[,2], y=rangey[,2], area=rangex[,2]*rangey[,2])
plotSizes


###################################################
### code chunk number 3: GeneratingPlotMaps2.Rnw:48-66
###################################################


##find center of map for our plot size
plotSizes$centerx <- NA
plotSizes$centery <- NA

plotSizes[,c("centerx", "centery")] <- findPlotStart(300, 300, plotSizes$x, plotSizes$y)

## add those values to rotPlots by plot.
##
## for each plot...

colnames(plotSizes) <- c("plot", "rangex", "rangey", "area", "centerx", "centery")

newRots <- merge(plotSizes[,c(1, 4:6)], rotPlots, by=c("plot"))

head(newRots)
tail(newRots)


###################################################
### code chunk number 4: GeneratingPlotMaps2.Rnw:71-106
###################################################

## We need to remove any records beyond the first years for our plots.

keepme <- vector()
##for each plot...
for(i in 1:length(plotnames)){
  minyear <- NULL
  minyear <- min(newRots[newRots$plot==plotnames[i], "measyear"])
  keepme <- c(keepme,
              which(newRots$plot==plotnames[i] &
                      newRots$measyear==minyear &
                      newRots$firstrec==minyear))

}

#X, Y, Species, Type, Diam, Height

sortieTrees <- data.frame(X=newRots[keepme, "x"],
                          Y=newRots[keepme, "y"],
                          Species=newRots[keepme, "species"],
                          Type=newRots[keepme, "stage"],
                          Diam=newRots[keepme, "dbh"],
                          Height=0,
                          Plot=newRots[keepme, "plot"],
                          stringsAsFactors=F)

sortieTrees[sortieTrees$Type=="tree", "Type"] <- "Adult"
sortieTrees[sortieTrees$Type=="seedling", "Type"] <- "Seedling"

sortieTrees[sortieTrees$Type=="Seedling", "Diam"] <- 1
##arbitrary, because seedlings in USGS plots don't have a dbh or db10
##measurement.

head(sortieTrees)
tail(sortieTrees)


###################################################
### code chunk number 5: GeneratingPlotMaps2.Rnw:112-145
###################################################

sortiePlotTreeNum <- aggregate(sortieTrees$Diam, by=list(sortieTrees$Plot, sortieTrees$Species), FUN=length)
colnames(sortiePlotTreeNum) <- c("Plot", "Species", "Count")
colnames(plotSizes)[1] <- c("Plot")
sortiePlotTreeNum <- merge(sortiePlotTreeNum, plotSizes[,c(1,4)], by=c("Plot"))
sortiePlotTreeNum$density <- sortiePlotTreeNum$Count / sortiePlotTreeNum$area

sortieTreesDBH <- aggregate(log(sortieTrees$Diam), by=list(sortieTrees$Plot, sortieTrees$Species), FUN=bootMean)
sortieTreesDBH$sddbh <- aggregate(log(sortieTrees$Diam), by=list(sortieTrees$Plot, sortieTrees$Species), FUN=bootSd)[,3]
sortieTreesDBH$counts <- aggregate(sortieTrees$Diam, by=list(sortieTrees$Plot, sortieTrees$Species), FUN=length)[,3]

sortieTreesDBH[is.na(sortieTreesDBH$sddbh),"sddbh"] <- 0
colnames(sortieTreesDBH) <- c("Plot", "Species", "meandbh", "sddbh", "n")

sortieTreesFinal <- merge(sortieTreesDBH, sortiePlotTreeNum[,c("Plot", "Species", "density")], by=c("Plot", "Species"))
sortieTreesFinal$giantn <- floor(sortieTreesFinal$density * (300*300))

response <- data.frame()
for(i in 1:nrow(sortieTreesFinal)){
  newresponse <- data.frame(Plot=sortieTreesFinal[i, "Plot"],
                            Species=sortieTreesFinal[i, "Species"],
                            X = runif(sortieTreesFinal[i, "giantn"], 0, 300),
                            Y = runif(sortieTreesFinal[i, "giantn"], 0, 300),
                            Type = "Adult",
                            Diam = rlnorm(sortieTreesFinal[i, "giantn"],
                                          sortieTreesFinal[i, "meandbh"],
                                          sortieTreesFinal[i, "sddbh"]))
  response <- rbind(response, newresponse)
}

##reorder response
response <- response[,c("X", "Y", "Species", "Type", "Diam", "Plot")]
response$Height <- 0


###################################################
### code chunk number 6: GeneratingPlotMaps2.Rnw:150-175
###################################################
## reset newresponse
newresponse <- data.frame()
for(i in 1:nrow(plotSizes)){
  newresponse <- rbind(newresponse,
                       response[response$Plot==plotSizes[i, "Plot"] &
                                   !(response$X < plotSizes[i, "rangex"] &
                                  response$Y < plotSizes[i, "rangey"]),])
}

nrow(response)
nrow(newresponse)


## show that we've deleted the right points
plot(newresponse$X, newresponse$Y, pch=".")

##combine
finalresponse <- rbind(newresponse, sortieTrees)
head(finalresponse)
tail(finalresponse)
nrow(finalresponse)
response <- finalresponse
response$X <- round(response$X, 2)
response$Y <- round(response$Y, 2)
response$Diam <- round(response$Diam, 2)


###################################################
### code chunk number 7: GeneratingPlotMaps2.Rnw:179-185
###################################################

crackers <- response[response$Plot=="crackers",]
plot(crackers$X,
     crackers$Y,
     col=as.factor(crackers$Species),
     pch=".")


###################################################
### code chunk number 8: GeneratingPlotMaps2.Rnw:188-193
###################################################
trinity <- response[response$Plot=="trinity",]
plot(trinity$X,
     trinity$Y,
     col=as.factor(trinity$Species),
     pch=".")


###################################################
### code chunk number 9: GeneratingPlotMaps2.Rnw:196-201
###################################################
realtor <- response[response$Plot=="realtor",]
plot(realtor$X,
     realtor$Y,
     col=as.factor(realtor$Species),
     pch=".")


###################################################
### code chunk number 10: GeneratingPlotMaps2.Rnw:204-209
###################################################
bellow <- response[response$Plot=="bellow",]
plot(bellow$X,
     bellow$Y,
     col=as.factor(bellow$Species),
     pch=".")


###################################################
### code chunk number 11: GeneratingPlotMaps2.Rnw:212-217
###################################################
reclusive <- response[response$Plot=="reclusive",]
plot(reclusive$X,
     reclusive$Y,
     col=as.factor(reclusive$Species),
     pch=".")


###################################################
### code chunk number 12: GeneratingPlotMaps2.Rnw:220-225
###################################################
octane <- response[response$Plot=="octane",]
plot(octane$X,
     octane$Y,
     col=as.factor(octane$Species),
     pch=".")


###################################################
### code chunk number 13: GeneratingPlotMaps2.Rnw:228-233
###################################################
sodium <- response[response$Plot=="sodium",]
plot(sodium$X,
     sodium$Y,
     col=as.factor(sodium$Species),
     pch=".")


###################################################
### code chunk number 14: GeneratingPlotMaps2.Rnw:236-241
###################################################
distress <- response[response$Plot=="distress",]
plot(distress$X,
     distress$Y,
     col=as.factor(distress$Species),
     pch=".")


###################################################
### code chunk number 15: GeneratingPlotMaps2.Rnw:244-249
###################################################
gravy <- response[response$Plot=="gravy",]
plot(gravy$X,
     gravy$Y,
     col=as.factor(gravy$Species),
     pch=".")


###################################################
### code chunk number 16: GeneratingPlotMaps2.Rnw:252-257
###################################################
trigger <- response[response$Plot=="trigger",]
plot(trigger$X,
     trigger$Y,
     col=as.factor(trigger$Species),
     pch=".")


###################################################
### code chunk number 17: GeneratingPlotMaps2.Rnw:260-265
###################################################
rigid <- response[response$Plot=="rigid",]
plot(rigid$X,
     rigid$Y,
     col=as.factor(rigid$Species),
     pch=".")


###################################################
### code chunk number 18: GeneratingPlotMaps2.Rnw:270-282
###################################################

 write.table(crackers[,-c(6)], file="crackers.txt", sep="\t", row.names=FALSE, quote=F)
 write.table(trinity[,-c(6)], file="trinity.txt", sep="\t", row.names=FALSE, quote=F)
 write.table(realtor[,-c(6)], file="realtor.txt", sep="\t", row.names=FALSE, quote=F)
 write.table(bellow[,-c(6)], file="bellow.txt", sep="\t", row.names=FALSE, quote=F)
 write.table(reclusive[,-c(6)], file="reclusive.txt", sep="\t", row.names=FALSE, quote=F)
 write.table(octane[,-c(6)], file="octane.txt", sep="\t", row.names=FALSE, quote=F)
 write.table(sodium[,-c(6)], file="sodium.txt", sep="\t", row.names=FALSE, quote=F)
 write.table(distress[,-c(6)], file="distress.txt", sep="\t", row.names=FALSE, quote=F)
 write.table(gravy[,-c(6)], file="gravy.txt", sep="\t", row.names=FALSE, quote=F)
 write.table(trigger[,-c(6)], file="trigger.txt", sep="\t", row.names=FALSE, quote=F)
 write.table(rigid[,-c(6)], file="rigid.txt", sep="\t", row.names=FALSE, quote=F)


