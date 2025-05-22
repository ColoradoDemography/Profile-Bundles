#' acsSummaryFile iReads the ACS Table Based Summary files from the SDO K: Drive  USED in the ACS based data applications
#'
#' @param tab is the list ACS Tables to extract
#' @param locList the list of selected locations (fips code) typically this is a list of 
#'       the US data, Colorado Data and the 
#' @param yr the year of the ACS data
#' @param dataset one year (1) or five year (5) data
#' @param outputMat the matrix of data objects
#' @export

# ACS 2023 Read Summary Files
# Reads a single summary file tab from the ACS FTP site
# Selects out geographies, "state", "colo" for both (colo filters out Colorado and County Records)
# Reformat names
# Support Functions
any_column_NA <- function(x){
  any(x < 0)
}
replace_NA_0 <- function(x){
  if_else(x < 0,NA,x)
}

acsSummaryfile <- function(tab,loclist,yr,dataset) {
  # this uses 
  # for earlier files, use TidyCensus of Census API
  
  acsbase <- paste0("J:/ACS/Summary_Files/",yr."...")
  f.dat <- read_delim(acscall, delim="|", col_names = TRUE)
  
  
  
  
  if(geo == "state") {
    f.dat2 <- f.dat %>%
      mutate(GEOST = str_sub(GEO_ID,1,9),
             GEOID = str_sub(GEO_ID,10,12)) %>%
      mutate_if(any_column_NA,replace_NA_0) %>%
      filter(GEOST == "0400000US") %>%
      filter((!GEOID %in% c("11","72"))) %>%
      mutate(GEOID = paste0(GEOID,"000"))
  }
  if(geo == "colo") {
    f.dat2 <- f.dat %>%
      mutate(GEOPRE = str_sub(GEO_ID,1,11),
             GEOID = str_sub(GEO_ID,10,15)) %>%
      mutate_if(any_column_NA,replace_NA_0) %>%
      filter(GEOPRE == "0400000US08" | GEOPRE == "0500000US08") %>%  
      mutate(GEOID = ifelse(nchar(str_trim(GEOID)) == 2,paste0(GEOID,"000"),GEOID))
  }
  
  if(geo == "place") {
    f.dat2 <- f.dat %>%
      mutate(GEOPRE = str_sub(GEO_ID,1,11),
             GEOID = str_sub(GEO_ID,10,16)) %>%
      mutate_if(any_column_NA,replace_NA_0) %>%
      filter(GEOPRE == "1600000US08") %>%  
      mutate(GEOID = ifelse(nchar(str_trim(GEOID)) == 2,paste0(GEOID,"000"),GEOID))
  }
  
  # Modifying names
  nameList <- names(f.dat2)
  for(i in 1:length(nameList)) {
    if(nchar(tab1) == 6){
      if(grepl(tab1,nameList[i])) {
        varpre <- str_sub(nameList[i],1,7)
        varchar <- str_sub(nameList[i],8,8)
        varpost <- str_sub(nameList[i],9,11)
        nameList[i] <- paste0(varpre,varpost,varchar)
      }      
    } else {
      if(grepl(tab1,nameList[i])) {
        varpre <- str_sub(nameList[i],1,8)
        varchar <- str_sub(nameList[i],9,9)
        varpost <- str_sub(nameList[i],10,12)
        nameList[i] <- paste0(varpre,varpost,varchar)
      }
    }
  }
  names(f.dat2) <- nameList
  return(f.dat2)
}
