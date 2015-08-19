#' Import a SORTIE-ND .out File Into R
#'
#' This function removes the first 5 lines of an .out file and then imports it
#' into a data.frame for further work.
#'
#' @param filenames A vector or character string of the filenames and paths you
#'   want to import. Include path if you're not in directory where the files are
#'   located.
#' @return This function returns a single data.frame of all the values. If your
#'   input files are not the same column number, this will throw an error.
#'
#'
#' @examples
#'  # getOutFile("myoutfile.out")
#'  # getOutFile(c("myoutfile1.out", "myoutfile2.out"))
#' @export

getOutFile <- function(filenames, numsubplots=1){
  dat <- data.frame()
  linestodelete <- 4+numsubplots
  for(i in 1:length(filenames)){
    importme <- readLines(filenames[i])
    cutmeoff <- importme[-c(1:linestodelete)]
    storeme <- read.table(textConnection(cutmeoff),
                          header=T,
                          stringsAsFactors=F,
                          sep="\t")
    dat <- rbind(dat, storeme)
  }
  return(dat)
}
