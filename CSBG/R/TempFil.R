#' TempFil Outputs a matrix of temporary file names and directories
#'
#' @param filemat Temporary file matrix
#' @return Matrix of filename vectors
#' @export

TempFil <- function(oDir,nfiles, ext) {
  oFile <- tempfile(tmpdir=oDir,fileext=ext)
 
return(oFile)  
}