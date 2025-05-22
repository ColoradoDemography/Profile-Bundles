#'clrGeoname A utility program clearing out the geoname variable for a selected DF
#'
#' @param inDF the dataframe to be processed
#' @param inVar variable to be cleared
#' @param npanel number of panels
#' @param posIncr The number of rows in each panel of inDF 
#' @return formatted datafile
#' @export
#'

clrGeoname <- function(inDF,inVar,npanel,posIncr) {

#Clearing selected vars
     
     startPos <- 2
     endPos <- startPos + (posIncr - 2)
    for(i in 1:npanel) {
     inDF[c(startPos:endPos),inVar] <- ""
      startPos <- startPos + posIncr
      endPos <- endPos + posIncr
    }
return(inDF)
   }