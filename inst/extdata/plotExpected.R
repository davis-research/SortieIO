### In this file, I need to develop some expected outcomes for SORTIE-ND. I need
### to generate the following statistics for each plot and time step, for
### seedlings, saplings, adults: Absolute density Relative density Absolute
### basal area Relative basal area
###
### SORTIE-ND counts saplings as anything taller than 1.35m but less wide than
### the defined minimum adult DBH  (10cm or manually set). Seedlings are
### anything less than 1.35m tall. So I'll need to incorporate that into my
### calculations for seedlings and saplings.
###
### Pseudo code time. Let's do start and end separately.
###
### For each plot,

library(MakeMyForests)
library(disperseR)

## Prep: Get the min and max years for each plot
plotYearRanges <- data.frame(plot=ssdPlotDesc$plot, minYear=ssdPlotDesc$yr1, maxYear=ssdPlotDesc$yr7, stringsAsFactors=F)
plotYearRanges[c(1:3, 9:11, 14), 3] <- ssdPlotDesc[c(1:3, 9:11, 14), "yr5"]
plotYearRanges[12:13, 3] <- ssdPlotDesc[12:13, "yr6"]

## Prep: Convert to basal area
expandedTrees$basalarea <- 0.00007854* (expandedTrees$dbh)^2
adultTrees <- expandedTrees[!is.na(expandedTrees$dbh),]
babyTrees <- expandedTrees[is.na(expandedTrees$dbh),]


## get absolute density by plot, species, year
plotCharacters <- aggregate(adultTrees$dbh, by=list(adultTrees$plot, adultTrees$species, adultTrees$measyear), FUN=length)
colnames(plotCharacters) <- c("plot", "species", "measyear", "absDens")
## get number of trees in a plot in a year
plotAllSppChars <- aggregate(adultTrees$dbh, by=list(adultTrees$plot, adultTrees$measyear), FUN=length)
colnames(plotAllSppChars) <- c("plot", "measyear", "totaln")

plotCharacters$absBasalArea <- aggregate(adultTrees$basalarea, by=list(adultTrees$plot, adultTrees$species, adultTrees$measyear), FUN=sum)[,4]
plotAllSppChars$totBasalArea <- aggregate(adultTrees$basalarea, by=list(adultTrees$plot, adultTrees$measyear), FUN=sum)[,3]

plotCharacters <- merge(plotCharacters, plotAllSppChars, by=c("plot", "measyear"), all=T)
plotCharacters$relDens <- plotCharacters$absDens/plotCharacters$totaln
plotCharacters$relBasalArea <- plotCharacters$absBasalArea/plotCharacters$totBasalArea

## Do the same for seedlings/saplings, except no basal area because dbh is NA

seedPlotChars <- aggregate(babyTrees$dbh, by=list(babyTrees$plot, babyTrees$measyear, babyTrees$species), FUN=length)
seedAllSppChars <- aggregate(babyTrees$dbh, by=list(babyTrees$plot, babyTrees$measyear), FUN=length)
seedPlotChars <- merge(seedPlotChars, seedAllSppChars, by=c("Group.1", "Group.2"), all=T)
colnames(seedPlotChars) <- c("plot", "measyear", "species", "absDens", "totaln")
seedPlotChars$relDens <- seedPlotChars$absDens/seedPlotChars$totaln
head(seedPlotChars)


## Combine the two into a larger dataframe to write
colnames(seedPlotChars) <- c("plot", "measyear", "species", "seedAbsDens", "totalSeeds", "seedRelDens")
colnames(plotCharacters) <- c("plot", "measyear", "species", "treeAbsDens", "treeAbsBasalArea", "totalTrees", "totalBasalArea", "treeRelDens", "treeRelBasalArea")

finalPlotChars <- merge(plotCharacters, seedPlotChars, by=c("plot", "measyear", "species"), all=T)

## begin prep to write to file
finalPlotChars$plot <- as.factor(finalPlotChars$plot)
finalPlotChars$species <- as.factor(finalPlotChars$species)

library(EML)
colDefs <- c("Plot name", "Measurement year", "Species shortcode", "Species absolute density", "Species absolute basal area", "Total plot number of trees", "Total plot basal area", "Species relative density", "Species relative basal area", "Seed absolute density", "Total number of seeds in plot", "Seed relative density")
length(colDefs)
ncol(finalPlotChars)

unitDefs <- list(
  c(bellow = "EMRIDGE",
    crackers = "BBBPIPO",
    distress = "POFLABMA",
    gravy = "SFTRABMA",
    octane = "FRPIJE",
    palate = "EMSALIX",
    realtor = "CRCRPIPO",
    reclusive = "EMSLOPE",
    rigid = "SUPILA",
    sodium = "PGABMA",
    trigger = "SUABCO"),
  c(unit="nominalYear"),
  c(ABCO = "Abies concolor concolor",
    ABMA = "Abies magnifica",
    CADE = "Calocedrus decurrens",
    PICO = "Pinus contorta",
    PIJE = "Pinus jeffreyi",
    PILA = "Pinus lambertiana",
    PIMO = "Pinus monticola",
    PIPO = "Pinus ponderosa",
    QUCH = "Quercus chrysolepis",
    QUKE = "Quercus kelloggii"),
  c(unit="number"),
  c(unit="squareMeter"),
  c(unit="number"),
  c(unit="squareMeter"),
  c(unit="number"),
  c(unit="squareMeter"),
  c(unit="number"),
  c(unit="number"),
  c(unit="number")
)

eml_write(finalPlotChars, col.defs=colDefs, unit.defs=unitDefs, creator="Sam Davis <sam@ecology.rocks>", file="expectedPlotOutcomes.xml")

eml_validate("expectedPlotOutcomes.xml")
write.csv(finalPlotChars, "expectedPlotOutcomes.csv")
