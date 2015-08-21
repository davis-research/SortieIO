## resample plots
sortieTrees$Plot <- as.character(sortieTrees$Plot)

## sortieTrees is the DF we need.
plotspecies <- unique(sortieTrees[, c("Plot", "Species")])
nrow(plotspecies)


newdf <- data.frame()
for(i in 1:nrow(plotspecies)){
  ## sample
  ## get number for 1 hectare...
  numrows <- nrow(sortieTrees[sortieTrees$Plot==plotspecies[i, 1] & sortieTrees$Species==plotspecies[i, 2], ] )
  ## multiply by 9...
  numrows <- numrows * 9
  ## sample
  diaval <- sample(sortieTrees[sortieTrees$Plot==plotspecies[i, 1] & sortieTrees$Species==plotspecies[i, 2], "Diam"],
                   numrows, replace=T)
  multiplier <- runif(numrows, 0.75, 1.25)
  diaval <- diaval * multiplier
  newdf <- rbind(newdf, data.frame(X=round(runif(numrows, 0, 300), 3),
                                   Y=round(runif(numrows, 0, 300), 3),
                                   Species=plotspecies[i, 2],
                                   Type="Adult",
                                   Diam=round(diaval, 3),
                                   Height=0,
                                   Plot=plotspecies[i, 1],
                                   stringsAsFactors = F))
  ## randomize
}

justplots <- unique(sortieTrees$Plot)
for(i in 1:length(justplots)){
  write.table(newdf[newdf$Plot==justplots[i], 1:6], file=paste("new-", justplots[i], ".txt", sep=""), quote=F, row.names=F, sep="\t")
}

save(newdf, file="newdf.RData")
