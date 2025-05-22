#' tabTitle manages the output of descriptive tabs in the interface
#'
#' @param item the item name in input$outChk
#' @return  Descriptive string provided in the tabs of the main interface
#' @export
#'
tabTitle <-function(item) {
  outTitle <- switch(item,
                      "age" = "Population by Age",
                     "ageemp" = "Unemployment Rate",
                     "pov" = "Population by Federal Poverty Level",
                     "educatt" = "Educational Attainment by Federal Poverty Level",
                     "povage" = "Age by Federal Poverty Level",
                     "povagetr" = "Age by Federal Poverty Level Trend",
                     "povagedis" = "Age by Federal Poverty Level for Persons with Disabilities",
                     "hhpov" = "Families by Type and Poverty Status",
                     "tenure" = "Housing Tenure by Poverty Status",
                     "snap" = "Supplemental Nutrition Assistance Program (SNAP)",
                     "wic" = "Women, Infants and Children (WIC)",
                     "insurance" = "Health Insurance by Age and Poverty Level"
)
  return(outTitle)
}
