#' captionSrc formats the captions for the tables and plots
#'
#' @param type the data type, "SDO" for State Demography Office data, "ACS" for American Community Survey data
#' @param dataSrc The data string for ACS data
#' @return  Date-stamped caption string
#' @export
#'
captionSrc <- function(type, dateSrc,tab) {


  if(type == "SDO") {
    srcStr <- paste0("State Demography Office Population Estimates and Forecasts Vintage ", dateSrc)
  }
  if(type == "SDOBEA") {
    srcStr <- paste0("State Demography Office and U.S. Bureau of Economic Analysis")
  }
  if(type == "SAIPE") {
    srcStr <- paste0("U.S. Census Bureau Small Area Income and Poverty Estimates (SAIPE), ", dateSrc)
  }
 
  if(type == "BLS2") {
    srcStr <- paste0("Bureau of Labor Statistics,\nLocal Area Unemployment Statistics ", dateSrc)
  }
  if(type == "BLS") {
    srcStr <- paste0("Bureau of Labor Statistics, Local Area Unemployment Statistics ", dateSrc)
  }
  if(type =="ACS") {
    byr <- dateSrc - 4
    eyr <- dateSrc
    srcStr <- paste0("U.S. Census Bureau, ",byr,"-",eyr," American Community Survey 5-Year Data, Table Number ",tab)
    
  }
  return(srcStr)
}
