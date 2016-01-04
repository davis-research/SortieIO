# SortieOutputs

This is an R package for parsing SORTIE-ND summary output files (.out) into human and R- readable formats for later analysis. I am building this package because I want to complete multiple runs of models and then analyze how stable their outputs are. Also, it's a good idea, since SORTIE-ND's file i/o leaves something to be desired.

Because this is an R package, you can install it directly from the R console using the following code:

```
install.packages("devtools")
library(devtools)
install_github("ecology-rocks/SortieIO")
```

Use the code documentation to explore functions, or see how it is used in the (http://www.github.com/ecology-rocks/SortieAnalysis)[SortieAnalysis repository]. The primary function used is processTestPlot(), which takes several parameters. Most of the other functions in the SortieIO package support the processTestPlot function. 

This R package may break easily. Do not be offended. Just work outside of the processTestPlot function to figure out what you want to do, and explore the R/ directory for the code behind my custom functions.