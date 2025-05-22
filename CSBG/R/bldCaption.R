
bldCaption <- function(inCaption){
           stmt1 <- "function(settings){"
           stmt2 <- "  var datatable = settings.oInstance.api();"
           stmt3 <- "  var table = datatable.table().node();"
           stmt4 <-  paste0("  var caption = '",inCaption,"'")
           stmt5 <-  "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');"
           stmt6 <- "}"
           
     outCap <- c(stmt1,stmt2,stmt3,stmt4,stmt5,stmt6)
     return(outCap)
}