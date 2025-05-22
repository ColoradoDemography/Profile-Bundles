#' povertyT6 generaes tables and ployly charts from SAIPE county data 2007-2017
#'  CSBG Dashboard 11/2019  A. Bickford
#' @param DBPool the DOLA database pool
#' @lvl the selected agenct
#' @param listID is the list of selected county codes
#' @return plotly graphic, data table and data file
#' @export

povertyT6 <- function(DBPool,lvl,listID){

  # Collecting List of Counties
 
  ctyfips <- as.numeric(substr(listID$list1,3,5))
  

 
# Extracting SAIPE data
  SAIPESQL <- "SELECT * FROM data.saipe;"
  f.saipe <- dbGetQuery(DBPool, SAIPESQL) %>% filter(fips %in% ctyfips)
  
   f.saipectyVAL <- f.saipe %>%
       mutate(popgt17 = totpop - pople17,
              poplt5 = pople17  - pop0517,
              povgt17 = povpop - povle17,
              povlt5 = povle17  - pov0517,
              povpctlt5 = povlt5/poplt5,
              povpct0517 = pov0517/pop0517,
              povpctgt17 = povgt17/popgt17,
              povpcttot = povpop/totpop)
   
   f.saipectyVAL <- f.saipectyVAL[,c("fips", "geoname", "year", 
                                     "poplt5","pop0517","popgt17", "totpop",
                                     "povlt5","pov0517", "povgt17", "povpop",
                                     "povpctlt5", "povpct0517", "povpctgt17","povpcttot")]
   
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
              poplt5 = pople17  - pop0517,
              povgt17 = povpop - povle17,
              povlt5 = povle17  - pov0517,
              povpctlt5 = povlt5/poplt5,
              povpct0517 = pov0517/pop0517,
              povpctgt17 = povgt17/popgt17,
              povpcttot = povpop/totpop)
     
    f.saipeagyVAL$fips <- 0
    f.saipeagyVAL$geoname <- listID$plName1
    
    f.saipeagyVAL <- f.saipeagyVAL[,c("fips", "geoname", "year", 
                                     "poplt5","pop0517","popgt17", "totpop",
                                     "povlt5","pov0517", "povgt17", "povpop",
                                     "povpctlt5", "povpct0517", "povpctgt17","povpcttot")]
    
    f.saipectyVAL <- bind_rows(f.saipeagyVAL, f.saipectyVAL)

   }

    
# creating Plotly Chart

    f.saipecty_PL <- gather(f.saipectyVAL[,c(1:3,12:14)],age_cat,value,povpctlt5:povpctgt17, factor_key=TRUE) 
    f.saipecty_PL$age_cat <- plyr::revalue(f.saipecty_PL$age_cat, c("povpctlt5" = "0 to 4",
                                                              "povpct0517" = "5 to 17",
                                                              "povpctgt17" = "18 and Older"))
    f.saipecty_PL$age_cat <- factor(f.saipecty_PL$age_cat, levels = c("0 to 4",
                                                              "5 to 17",
                                                              "18 and Older"))
  
    f.saipecty_PL$indText  <- paste0( f.saipectyVAL$geoname," Year: ",f.saipecty_PL$year," Ages: ",f.saipecty_PL$age_cat,", Percent in Poverty: ", percent( f.saipecty_PL$value * 100))  
    grTitle <- paste0("Table 6: Percent Below Federal Poverty Level by Age Trend, ",listID$plName1)
    outCap <- captionSrc("SAIPE","","")
    xAxis <- list(title = "Age Category")
    yAxis <- list(title = 'Percent',tickformat = ".1%")

if(length(ctyfips) > 1 ){
POVPlot <- plot_ly(f.saipecty_PL, 
                   x = ~year, 
                   y = ~value, 
                   name = ~age_cat, type = 'scatter', mode = 'lines', text = ~indText,  textposition = "none",hoverinfo = 'text',
                   transforms = list(
                      list(
                        type = 'filter',
                        target = ~geoname,
                        operation = '=',
                        value = unique(f.saipecty_PL$geoname)[1]))) %>%
 layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4),
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PL$geoname)[1]),
                     label = unique(f.saipecty_PL$geoname)[1]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PL$geoname)[2]),
                     label = unique(f.saipecty_PL$geoname)[2]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PL$geoname)[3]),
                     label = unique(f.saipecty_PL$geoname)[3]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PL$geoname)[4]),
                     label = unique(f.saipecty_PL$geoname)[4]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PL$geoname)[5]),
                     label = unique(f.saipecty_PL$geoname)[5]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PL$geoname)[6]),
                     label = unique(f.saipecty_PL$geoname)[6]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PL$geoname)[7]),
                     label = unique(f.saipecty_PL$geoname)[7])
            )
        )))
} else {
   POVPlot <- plot_ly(f.saipecty_PL, 
                      x = ~year, y = ~value, name = ~age_cat, type = 'scatter',
                      mode = 'lines', text = ~indText,  textposition = "none",hoverinfo = 'text') %>%
     layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
   
}
    
# Creating Table data file
   
    f.saipe_POP <- f.saipectyVAL[,c(1:7)] 
    f.saipe_POP[,4:7] <- sapply(f.saipe_POP[,4:7],NumFmt) 
    f.saipe_POPW <- gather(f.saipe_POP,age_cat,value, poplt5:totpop,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_POPW$type = "Total Population"
    
    f.saipe_POV <- f.saipectyVAL[,c(1:3,8:11)] 
    f.saipe_POV[,4:7] <- sapply(f.saipe_POV[,4:7],NumFmt) 
    f.saipe_POVW <- gather(f.saipe_POV,age_cat,value, povlt5:povpop,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_POVW$type = "Population below FPL"
    
    f.saipe_PCT <- f.saipectyVAL[,c(1:3,12:15)] 
    
    f.saipe_PCT[,4:7] <- lapply(f.saipe_PCT[,4:7], function(x) x * 100)
    f.saipe_PCT[,4:7] <- sapply(f.saipe_PCT[,4:7],percent) 
    f.saipe_PCTW <- gather(f.saipe_PCT,age_cat,value, povpctlt5:povpcttot,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_PCTW$type = "Percentage"

    f.saipecty_tab <- bind_rows(f.saipe_PCTW, f.saipe_POVW, f.saipe_POPW) %>% arrange(fips,type)
    f.saipecty_tab <-f.saipecty_tab[,c(2,15,3,4:14)]
    
    f.saipecty_tab$age_cat <- plyr::revalue(f.saipecty_tab$age_cat, 
                  c("povpctlt5" = 	"0 to 4",
                    "povpct0517" = 	"5 to 17",
                    "povpctgt17" =  	"18 and Older",
                    "povpcttot" = "Total",
                    "povlt5" = 	"0 to 4",
                    "pov0517" = 	"5 to 17",
                    "povgt17" = 	"18 and Older",
                    "povpop" = 	"Total",
                    "poplt5" = 	"0 to 4",
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
    
    f.saipecty_tab <- clrGeoname(f.saipecty_tab,"geoname",npanel1,12)
    f.saipecty_tab <- clrGeoname(f.saipecty_tab,"type",npanel2,4)
    
    

 # Flex Table
  tab_head <- paste0("Table 6: Percent Below Federal Poverty Level by Age Trend, ",listID$plName1)
  
  
  f.povFlex <- flextable(
       f.saipecty_tab,
       col_keys = names(f.saipecty_tab)) %>%
       set_header_labels(geoname = "Agency/County", type= "Data Type", age_cat = "Age Category") %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=14) %>%
       add_footer_row(values=captionSrc("SAIPE","",""),top=FALSE,colwidths=14) 
      

  names(f.saipecty_tab)[1] <- "Agency/County"
  names(f.saipecty_tab)[1] <- " Data Type"
  names(f.saipecty_tab)[3] <- "Age Category"
    
    

  

  #bind list
  outList <- list("plot"= POVPlot, "data" = f.saipecty_tab, "FlexTable" = f.povFlex, "caption" = outCap)
  
  return(outList)
}