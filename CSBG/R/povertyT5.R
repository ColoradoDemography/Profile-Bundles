#' povertyT5 generaes tables and ployly charts from SAIPE county data for the current year
#'  CSBG Dashboard 11/2019  A. Bickford
#' @param DBPool the DOLA database pool
#' @lvl the selected agency
#' @param listID is the list of selected county codes
#' @param cyrYr Current year value
#' @return plotly graphic, data table and data file
#' @export

povertyT5 <- function(DBPool,lvl,listID,curYR){

  # Collecting List of Counties
 
  ctyfips <- as.numeric(substr(listID$list1,3,5))
  
  
 
# Extracting SAIPE data
  SAIPESQL <- "SELECT * FROM data.saipe;"
  f.saipe <- dbGetQuery(DBPool, SAIPESQL) %>% filter(fips %in% ctyfips  & year == curYR)
  
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

    f.saipecty_PL <- gather(f.saipectyVAL[,c(1:3,10:12)],age_cat,value,povpct0517:povpcttot, factor_key=TRUE) 
    f.saipecty_PL$age_cat <- plyr::revalue(f.saipecty_PL$age_cat, c("povpct0517" = "5 to 17",
                                                              "povpctgt17" = "18 and Older",
                                                              "povpcttot" = "All Persons"))
    f.saipecty_PL$age_cat <- factor(f.saipecty_PL$age_cat, levels = c("5 to 17",
                                                              "18 and Older", "All Persons"))
  
    f.saipecty_PL$indText  <- paste0( f.saipectyVAL$geoname," Year: ",f.saipecty_PL$year," Ages: ",f.saipecty_PL$age_cat,", Percent in Poverty: ", percent( f.saipecty_PL$value * 100))  
    grTitle <- paste0("Table 5: Percent Below Federal Poverty Level, ",listID$plName1," ",curYR)
    outCap <- captionSrc("SAIPE","","")
    xAxis <- list(title = "Age Category")
    yAxis <- list(title = 'Percent',tickformat = ".1%")

if(length(ctyfips) > 1 ){
POVPlot <- plot_ly(f.saipecty_PL, 
                   x = ~age_cat, 
                   y = ~value, 
                   type = 'bar', text = ~indText,  textposition = "none", hoverinfo = 'text',
                   transforms = list(
                      list(
                        type = 'filter',
                        target = ~geoname,
                        operation = '=',
                        value = unique(f.saipecty_PL$geoname)[1]))) %>%
 layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
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
                      x = ~age_cat, y = ~value,  type = 'bar',
                      text = ~indText,  textposition = "none", hoverinfo = 'text') %>%
    layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
   
}
    
# Creating Table data file

    f.saipe_POP <- f.saipectyVAL[,c(1:6)] 
    f.saipe_POP[,4:6] <- sapply(f.saipe_POP[,4:6],NumFmt) 
    f.saipe_POPW <- gather(f.saipe_POP,age_cat,value, pop0517:totpop,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_POPW$type = "Total Population"
    f.saipe_POPW$age_cat <- as.character(f.saipe_POPW$age_cat)
    
    f.saipe_POV <- f.saipectyVAL[,c(1:3,7:9)] 
    f.saipe_POV[,4:6] <- sapply(f.saipe_POV[,4:6],NumFmt) 
    f.saipe_POVW <- gather(f.saipe_POV,age_cat,value, pov0517:povpop,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_POVW$type = "Population Pelow FPL"
    f.saipe_POVW$age_cat <- as.character(f.saipe_POVW$age_cat)
    
    f.saipe_PCT <- f.saipectyVAL[,c(1:3,10:12)] 
    f.saipe_PCT[,4:6] <- lapply(f.saipe_PCT[,4:6], function(x) x * 100)
    f.saipe_PCT[,4:6] <- sapply(f.saipe_PCT[,4:6],percent) 
    f.saipe_PCTW <- gather(f.saipe_PCT,age_cat,value, povpct0517:povpcttot,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_PCTW$type = "Percentage Below FPL"
    f.saipe_PCTW$age_cat <- as.character(f.saipe_PCTW$age_cat)
    

    f.saipecty_tab <- bind_rows(f.saipe_PCTW, f.saipe_POVW, f.saipe_POPW) %>% arrange(fips,type)
    f.saipecty_tab <-f.saipecty_tab[,c(1,2,5,3,4)]
    names(f.saipecty_tab) <- c("fips","geoname","type", "age_cat","value")
    
    f.saipecty_tab$age_cat <- plyr::revalue(f.saipecty_tab$age_cat, 
                  c(
                    "povpct0517" = 	"5 to 17",
                    "povpctgt17" =  	"18 and Older",
                    "povpcttot" = "Total",
                    "pov0517" = 	"5 to 17",
                    "povgt17" = 	"18 and Older",
                    "povpop" = 	"Total",
                    "pop0517" = 	"5 to 17",
                    "popgt17" = 	"18 and Older",
                    "totpop" = 	"Total"))
    
    f.saipecty_tabW <-  f.saipecty_tab %>% spread(age_cat, value)
  
    
     #Clearing geoname
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
    
    f.saipecty_tabW <- clrGeoname(f.saipecty_tabW,"geoname",npanel1,3)
    f.saipecty_tabW <- f.saipecty_tabW[,c(2:6)]
    names(f.saipecty_tabW)[1] <- "Agency/County"
    names(f.saipecty_tabW)[2] <- "Data Type"

 # Flex Table
  tab_head <- paste0("Table 5: Percent Below Federal Poverty Level, ",listID$plName1, " ",curYR)
  
  
  f.povFlex <- flextable(
       f.saipecty_tabW,
       col_keys = names(f.saipecty_tabW)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=5) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=5) 
      

  
    

  

  #bind list
  outList <- list("plot"= POVPlot, "data" = f.saipecty_tabW, "FlexTable" = f.povFlex,"caption" = outCap)
  
  return(outList)
}