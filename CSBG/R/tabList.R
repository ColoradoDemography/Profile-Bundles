#' tabList returns the list items from the outputList  based on the value of input$outChk
#'
#' @param item the item name in input$outChk
#' @return the content of each panel, drawn from the output lists defined in the
#'      GLOBAL section of the  UI code
#' @export

tabList <- function(item){

  outList <- list("Section Not Defined")
  if(item == "age") {
    outList <- age.list
  }
  if(item == "ageemp") {
    outList <- emp.list
  }
  if(item == "educatt") {
    outList <- educ.list
  }
  if(item == "pov") {
    outList <- pov3.list
  }
   if(item == "povage") {
    outList <- pov5.list
   }
   if(item == "povagetr") {
    outList <- pov6.list
  }
  if(item == "povagedis") {
    outList <- dis7.list
  }
  if(item == "hhpov") {
    outList <- fam8.list
  }
  if(item == "tenure") {
    outList <- hh9.list
  }
  if(item == "snap") {
    outList <- snap10.list
  }
   if(item == "wic") {
    outList <- wic11.list
  }
  if(item == "insurance") {
    outList <- ins12.list
  }
  return(outList)
}

