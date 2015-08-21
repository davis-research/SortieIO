## read in files
##
## extending from newtreemapgen081315.R

load("newdf.RData")

justplots <- unique(sortieTrees$Plot)
## for each plot
pdf(file="treemaps.pdf")
for(i in 1:length(justplots)){
  tempplot <- newdf[newdf$Plot==justplots[i],]
  plot(1, type="n", xlim=c(0,300), ylim=c(0,300), xlab="West-East", ylab="South-North", main=justplots[i])
  symbols(tempplot$X, tempplot$Y, circles=tempplot$Diam/100, inches=F, add=T, fg=as.factor(tempplot$Species))
}
dev.off()

basalarea <- aggregate((newdf$Diam*newdf$Diam*0.00007854), by=list(newdf$Plot, newdf$Species), FUN=sum, na.rm=T)
basalarea$x <- format(basalarea$x/9, scientific=F)
head(basalarea)
colnames(basalarea) <- c("Plot", "Species", "BasalAreaPerHectare")
basalarea <- convertPlotNames(basalarea, "Plot", realnames=F)
write.csv(basalarea, file="new-basalareacheck.csv", row.names=F)
