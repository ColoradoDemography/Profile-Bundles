#' downloadObj  File Download Modules
#'
#' downloadObj is the server function that facilitates the download
#'
#' @param place is the place name, typically the value of input$unit
#' @param oname the input data description object
#' @param dobj is the data object to be output
#' @export

downloadObj <- function(input, output, session, place, oname, dobj) {

  if(nchar(oname) == 9) {
    dname <- substr(oname,1,5)
    dtype <- substr(oname,6,9)
  }

  if(nchar(oname) == 10) {
    dname <- substr(oname,1,6)
    dtype <- substr(oname,7,10)
  }


  prefix <- switch(dname,
                   "popa1" = " Age Distribution",
                   "pope1" = " Unemployment Rate",
                   "povpp3" = " Population by Federal Poverty Level",
                   "poped1" = " Educational Attainment by Fed Poverty Level",
                   "povpp5" = " Age by Federal Poverty Level",
                   "povpp6" = " Age by Federal Poverty Level Trend",
                   "dispp7" = " Age by Federal Poverty Level for Persons with Disabilities",
                   "fampv8" = " Families by Type and Poverty Status",
                   "house9" = " Housing Tenure by Poverty",
                   "snap10" = " Supplemental Nutrition Assistance Program", 
                   "wic11"  = " Women Infants and Children",
                   "ins12" = " Health Insurance by Source"
  )

  suffix <- ifelse(dtype == "data"," Data.csv"," Table.docx")

  output$download <-  downloadHandler(
    filename = function() {
      paste0(prefix," ",place,suffix)
    },
    content = function(file) {
      if(suffix == " Data.csv") {
        write_csv(dobj, file)
      }
      if(suffix == " Table.docx") {
        doc <- read_docx()
        doc <-  body_add_flextable(doc, value = dobj) %>%
          body_end_section_landscape(w = 21/2.54, h = 29.7/2.54)
        
        print(doc, target = file)
      }
    } #content
  ) #DowhloadHandler
} #downloadObj
