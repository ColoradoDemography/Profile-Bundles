#' povertyTrend generaes tables and ployly charts from SAIPE county data ten year period
#'  CSBG Dashboard 11/2019  A. Bickford
#' @param DBPool the DOLA database pool
#' @param lvl the selected agency
#' @param listID is the list of selected county codes
#' @return plotly graphic, data table and data file
#' @export

povertyTrend <- function(lvl,listID,ACS,curYr,censKey){

  yrval10 <- curYr - 10
  # Collecting List of Counties
  timeSTR <- paste0("from ", yrval10," to ", curYr)

 
  ctyfips <- as.numeric(substr(listID$list1,3,5))
  
  f.saiperaw <- getCensus(key=censAPI,
                          name = "timeseries/poverty/saipe",
                          vars = c("STABREV", "YEAR", "GEOID", "NAME", 
                                   "SAEPOV0_17_PT", "SAEPOV5_17R_PT", "SAEPOVALL_PT",
                                   "SAEPOVU_0_17", "SAEPOVU_5_17R", "SAEPOVU_ALL"), 
                          region = "county:*", regionin= "state:08",
                          time = timeSTR) 


  f.saipe <- f.saiperaw %>% 
    filter(GEOID %in% listID$list1) %>%
    mutate(fips = as.numeric(county),
           geoname = NAME,
           year = YEAR,
           totpop = as.numeric(SAEPOVU_ALL),
           pople17 = as.numeric(SAEPOVU_0_17),
           pop0517 = as.numeric(SAEPOVU_5_17R),
           povpop  = as.numeric(SAEPOVALL_PT),
           povle17 = as.numeric(SAEPOV0_17_PT),
           pov0517 = as.numeric(SAEPOV5_17R_PT)
    )
 

  
   f.saipectyVAL <- f.saipe %>%
       mutate(popgt17 = totpop - pople17,
              povgt17 = povpop - povle17,
              povpct0517 = pov0517/pop0517,
              povpctgt17 = povgt17/popgt17,
              povpcttot = povpop/totpop)
   
   f.saipectyVAL <- f.saipectyVAL[,c("fips", "geoname", "year", 
                                     "pop0517","popgt17", "totpop",
                                     "pov0517", "povgt17", "povpop",
                                     "povpct0517", "povpctgt17","povpcttot")]
   
   if(length(ctyfips) > 1){
    f.saipeagyVAL <- f.saipe %>%
       group_by(year) %>%
       summarize(totpop = sum(totpop),
                 povpop	= sum(povpop),
                 pople17 = sum(pople17),
                 povle17 = sum(povle17),
                 pop0517 = sum(pop0517),
                 pov0517 = sum(pov0517))  %>%
       mutate(popgt17 = totpop - pople17,
              povgt17 = povpop - povle17,
              povpct0517 = pov0517/pop0517,
              povpctgt17 = povgt17/popgt17,
              povpcttot = povpop/totpop)
     
    f.saipeagyVAL$fips <- 0
    f.saipeagyVAL$geoname <- listID$plName1
    
    f.saipeagyVAL <- f.saipeagyVAL[,c("fips", "geoname", "year", 
                                     "pop0517","popgt17", "totpop",
                                     "pov0517", "povgt17", "povpop",
                                     "povpct0517", "povpctgt17","povpcttot")]
    
    f.saipectyVAL <- bind_rows(f.saipeagyVAL, f.saipectyVAL)

   }

    
# creating Plotly Chart
   f.saipecty_CL <- gather(f.saipectyVAL[,c(1:3,7:9)],age_cat,count,pov0517:povpop, factor_key=TRUE) 
    f.saipecty_CL$age_cat <- plyr::revalue(f.saipecty_CL$age_cat, c("pov0517" = "5 to 17",
                                                              "povgt17" = "18 and Older",
                                                              "povpop" = "All Persons"))
    f.saipecty_CL$age_cat <- factor(f.saipecty_CL$age_cat, levels = c("5 to 17",
                                                              "18 and Older", "All Persons"))


    f.saipecty_PL <- gather(f.saipectyVAL[,c(1:3,10:12)],age_cat,value,povpct0517:povpcttot, factor_key=TRUE) 
    f.saipecty_PL$age_cat <- plyr::revalue(f.saipecty_PL$age_cat, c("povpct0517" = "5 to 17",
                                                              "povpctgt17" = "18 and Older",
                                                              "povpcttot" = "All Persons"))
    f.saipecty_PL$age_cat <- factor(f.saipecty_PL$age_cat, levels = c("5 to 17",
                                                              "18 and Older", "All Persons"))

    f.saipecty_PLOT <- inner_join(f.saipecty_PL,f.saipecty_CL[,c(1,3:5)], by=c("fips","year","age_cat"))
    f.saipecty_PLOT$indText  <- paste0( f.saipectyVAL$geoname," Year: ",f.saipecty_PLOT$year," Ages: ",f.saipecty_PLOT$age_cat,", Percent in Poverty: ", percent(f.saipecty_PLOT$value * 100)," Estimate: ",NumFmt(f.saipecty_PLOT$count))  
    grTitle <- paste0("Percent Below Federal Poverty Level by Age Trend, ",listID$plName1)
    outCap <- captionSrc("SAIPE","","")
    xAxis <- list(title = "Age Category")
    yAxis <- list(title = 'Percent',tickformat = ".1%")
    txtNames <- unique(f.saipecty_PLOT$geoname)

if(length(ctyfips) > 1 ){
POVPlot <- plot_ly(f.saipecty_PLOT, 
                   x = ~year, 
                   y = ~value, 
                   name = ~age_cat, type = 'scatter', mode = 'lines', text = ~indText,  textposition = "none",hoverinfo = 'text',
                   transforms = list(
                      list(
                        type = 'filter',
                        target = ~geoname,
                        operation = '=',
                        value = unique(f.saipecty_PLOT$geoname)[1]))) %>%
 layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4),
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = genDropdown(txtNames)
        )))
} else {
   POVPlot <- plot_ly(f.saipecty_PLOT, 
                      x = ~year, y = ~value, name = ~age_cat, type = 'scatter',
                      mode = 'lines', text = ~indText,  textposition = "none",hoverinfo = 'text') %>%
     layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
   
}
    
# Creating Table data file

    f.saipe_POP <- f.saipectyVAL[,c(1:6)] 
    f.saipe_POP[,4:6] <- sapply(f.saipe_POP[,4:6],NumFmt) 
    f.saipe_POPW <- gather(f.saipe_POP,age_cat,value, pop0517:totpop,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_POPW$type = "Total Population"
    
    f.saipe_POV <- f.saipectyVAL[,c(1:3,7:9)] 
    f.saipe_POV[,4:6] <- sapply(f.saipe_POV[,4:6],NumFmt) 
    f.saipe_POVW <- gather(f.saipe_POV,age_cat,value, pov0517:povpop,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_POVW$type = "Population below FPL"
    
    f.saipe_PCT <- f.saipectyVAL[,c(1:3,10:12)] 
    
    f.saipe_PCT[,4:6] <- lapply(f.saipe_PCT[,4:6], function(x) x * 100)
    f.saipe_PCT[,4:6] <- sapply(f.saipe_PCT[,4:6],percent) 
    f.saipe_PCTW <- gather(f.saipe_PCT,age_cat,value, povpct0517:povpcttot,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_PCTW$type = "Percentage"

    f.saipecty_tab <- bind_rows(f.saipe_PCTW, f.saipe_POVW, f.saipe_POPW) %>% arrange(fips,type) 
    f.saipecty_tab <- f.saipecty_tab[,c(15,2:14)]
   
    
    
    f.saipecty_tab$age_cat <- plyr::revalue(f.saipecty_tab$age_cat, 
                  c("povpct0517" = 	"5 to 17",
                    "povpctgt17" =  	"18 and Older",
                    "povpcttot" = "Total",
                    "pov0517" = 	"5 to 17",
                    "povgt17" = 	"18 and Older",
                    "povpop" = 	"Total",
                    "pop0517" = 	"5 to 17",
                    "popgt17" = 	"18 and Older",
                    "totpop" = 	"Total"))
    
    if(length(ctyfips) == 1) {
      npanel1 = 1
      npanel2 = 3
    } else {
      npanel1 <- length(ctyfips) + 1
      npanel2 = 3 * npanel1
    }
    
    f.saipecty_tab <- clrGeoname(f.saipecty_tab,"geoname",npanel1,9)
    f.saipecty_tab <- clrGeoname(f.saipecty_tab,"type",npanel2,3)
    
    

 # Flex Table

  tab_head <- paste0("Percent Below Federal Poverty Level by Age Trend, ",listID$plName1)
  nCols <- ncol(f.saipecty_tab)
   names(f.saipecty_tab)[1] <- "Agency/County"
  names(f.saipecty_tab)[2] <- "Value"
  names(f.saipecty_tab)[3] <- "Age Category"
  
  f.povFlex <- flextable(
       f.saipecty_tab,
       col_keys = names(f.saipecty_tab)) %>%
       fontsize(size=10, part="all") %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=nCols) %>%
       add_footer_row(values=captionSrc("SAIPE","",""),top=FALSE,colwidths=nCols) %>%
       align(j=1:2, align="left", part="body") %>%
       width(j= 1, width=2) %>%
       width(j=2:3, width=1) %>%
       width(j=4:11,width=0.6) %>%
       height(part="footer", height=0.4) %>%
       height(part="body", height=0.5)
      

 
    


  #bind list
  outList <- list("plot"= POVPlot, "data" = f.saipecty_PLOT, "table" = f.saipecty_tab, "FlexTable" = f.povFlex, "caption" = outCap)
  
  return(outList)
}