#'agePoverty Outputs Table of Age by Poverty stats and charts
#'  This produces output for
#'  Table 5 Households in poverty
#'  Table 6 Detailed Individual Poverty by age
#'  Table 7 Poverty Trend 
#'  Table 8 Poverty by Disability Status
#'    pulls data from ACS API 
#'
#'
#' @parap lvl the name of the agency 
#' @param listID the list containing place id and Place names
#' @param state is the state that the original fips
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @return Plotly Bar chart, Flextable and formatted data set
#' @export
#'
agePoverty <- function(lvl,listID, ACS,curYr,TabNo) {
  # Collecting place ids from  idList, setting default values

   ctyfips <- substr(listID$list1,3,5)
   state <- "08"
    AgeEmpCTY <- data.frame()
    for(i in 1:length(ctyfips)) {
      tempDF <- codemog_api(data="b23001",db=ACS,geonum=paste("1",state , ctyfips[i],sep=""),meta="no")
      AgeEmpCTY <- bind_rows(AgeEmpCTY,tempDF)
    }
 
    x <- names(AgeEmpCTY)
 

  
    #Preparing Flextable
    ACSName2 <- toupper(substr(ACS,1,3))
    ACSyr <- paste0("20",substr(ACS,4,5),"-","20",substr(ACS,6,7))
    Acs_Str <- paste0(ACSName2," ",ACSyr)
    tab_date <- substr(captionSrc("ACS",ACS),66,87)
    
    f.raceFlex <- as.data.frame(m.race)
    names(f.raceFlex) <- c("V1","V2","V3","V4","V5","V6","V7")
    FlexOut <- regulartable(f.raceFlex) %>%
      set_header_labels(  V1 = "Race Category", V2 = "Census 2000",
                          V3 = "Census 2010", V4 = Acs_Str,
                          V5 = "Census 2000",
                          V6 = "Census 2010", V7 = Acs_Str)
    
    
    if(nchar(placefips) == 0) {
      FlexOut <- add_header(FlexOut,V2=ctyname,V5="Colorado",top=TRUE)
    } else {
      FlexOut <- add_header(FlexOut,V2=placename,V5=ctyname,top=TRUE)
    }
    
    FlexOut <- FlexOut %>% add_header(V1="Race Trend",top=TRUE) %>%
      add_footer(V1=tab_date) %>%
      merge_at(i=2,j=2:4,part="header") %>%
      merge_at(i=2,j=5:7,part="header") %>%
      align(i=1, j=1, align="left",part="header") %>%
      align(i=2,j=2:4,align="center",part="header") %>%
      align(i=2,j=5:7,align="center",part="header") %>%
      align(i=3,j=1,align="left",part="header") %>%
      align(i=3,j=2:7,align="center",part="header") %>%
      align(i=1, align="left",part="footer") %>%
      align(j=1, align="left", part="body") %>%
      width(j=1, width=3.0) %>%
      width(j=2:7, width=0.8)
  
 
    
    #Preparing Text
    if(nchar(placefips) == 0) {
      OutText <- paste0("  The Race Trend table shows the changing racial and ethnic composition of ",ctyname," beginning in 2000 and continuing to the present.")
    } else {
      OutText <- paste0("  The Race Trend table shows the changing racial and ethnic composition of ",placename," beginning in 2000 and continuing to the present.")
    } 
  
  
  outList <- list("Htable" = tabHTML, "data" = race_data,"FlexTable"=FlexOut, "Ltable" = tabLATEX,"text" = OutText)
  return(outList)
}


