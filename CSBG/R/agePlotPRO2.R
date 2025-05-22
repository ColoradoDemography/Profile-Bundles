#' agePlotPRO Creates a Chart comparing The age distribution of a selected counties for a single year
#'
#' @param DBPool the active database pool
#' @param lvl is the data type 
#' @param listID is the list of selected county codes
#' @param curYr is the single year value to be extracted by county_sya
#' @return plotly graphic, data table and data file
#' @export

agePlotPRO2  <- function(lvl,listID,ACS,curYr,censKEY) {
  outCap <- captionSrc("ACS",ACS,"B01001")
  
  # Building data sets
  f.AgeUS <- get_acs(geography = 'us', table="B01001", 
                     survey = "acs5", year= curYr, output="wide") %>%
    mutate( GEOID = "01000",
            YEAR = curYr,
            totalpopulation = 	B01001_002E + B01001_026E,
            age0004 = 	B01001_003E + B01001_027E,
            age0517 = 	B01001_004E + B01001_028E + B01001_005E + B01001_029E +
                        B01001_006E + B01001_030E,
            age1844 = 	B01001_007E + B01001_031E +
              B01001_008E + B01001_032E	+
              B01001_009E + B01001_033E +
              B01001_010E + B01001_034E +
              B01001_011E + B01001_035E +
              B01001_012E + B01001_036E +
              B01001_013E + B01001_037E +
              B01001_014E + B01001_038E,
            age4564 = B01001_015E + B01001_039E +
              B01001_016E + B01001_040E +
              B01001_017E + B01001_041E +
              B01001_018E + B01001_042E +
              B01001_019E + B01001_043E,
            age65 = 	B01001_020E + B01001_044E +
              B01001_021E + B01001_045E +
              B01001_022E + B01001_046E +
              B01001_023E + B01001_047E +
              B01001_024E + B01001_048E +
              B01001_025E + B01001_049E,
            totalpopp = 1,
            age0004p =  age0004/totalpopulation,
            age0517p =  age0517/totalpopulation,
            age1844p =  age1844/totalpopulation,
            age4564p =  age4564/totalpopulation,
            age65p =  age65/totalpopulation) %>%
    select(GEOID, NAME, YEAR, totalpopulation:age65p)
  
  f.AgeST <- get_acs(geography = 'state', table="B01001", state="CO",
                     survey = "acs5", year= curYr, output="wide") %>%
    mutate( GEOID = "08000",
            YEAR = curYr,
            totalpopulation = 	B01001_002E + B01001_026E,
            age0004 = 	B01001_003E + B01001_027E,
            age0517 = 	B01001_004E + B01001_028E + B01001_005E + B01001_029E +
              B01001_006E + B01001_030E,
            age1844 = 	B01001_007E + B01001_031E +
              B01001_008E + B01001_032E	+
              B01001_009E + B01001_033E +
              B01001_010E + B01001_034E +
              B01001_011E + B01001_035E +
              B01001_012E + B01001_036E +
              B01001_013E + B01001_037E +
              B01001_014E + B01001_038E,
            age4564 = B01001_015E + B01001_039E +
              B01001_016E + B01001_040E +
              B01001_017E + B01001_041E +
              B01001_018E + B01001_042E +
              B01001_019E + B01001_043E,
            age65 = 	B01001_020E + B01001_044E +
              B01001_021E + B01001_045E +
              B01001_022E + B01001_046E +
              B01001_023E + B01001_047E +
              B01001_024E + B01001_048E +
              B01001_025E + B01001_049E,
            totalpopp = 1,
            age0004p =  age0004/totalpopulation,
            age0517p =  age0517/totalpopulation,
            age1844p =  age1844/totalpopulation,
            age4564p =  age4564/totalpopulation,
            age65p =  age65/totalpopulation) %>%
    select(GEOID, NAME, YEAR, totalpopulation:age65p)
  
  f.AgeCTY <- get_acs(geography = 'county', table="B01001", state="CO",
                     survey = "acs5", year= curYr, output="wide") %>%
              filter(GEOID %in% listID$list1) %>%
    mutate( YEAR = curYr,
            totalpopulation = 	B01001_002E + B01001_026E,
            age0004 = 	B01001_003E + B01001_027E,
            age0517 = 	B01001_004E + B01001_028E + B01001_005E + B01001_029E +
              B01001_006E + B01001_030E,
            age1844 = 	B01001_007E + B01001_031E +
              B01001_008E + B01001_032E	+
              B01001_009E + B01001_033E +
              B01001_010E + B01001_034E +
              B01001_011E + B01001_035E +
              B01001_012E + B01001_036E +
              B01001_013E + B01001_037E +
              B01001_014E + B01001_038E,
            age4564 = B01001_015E + B01001_039E +
              B01001_016E + B01001_040E +
              B01001_017E + B01001_041E +
              B01001_018E + B01001_042E +
              B01001_019E + B01001_043E,
            age65 = 	B01001_020E + B01001_044E +
              B01001_021E + B01001_045E +
              B01001_022E + B01001_046E +
              B01001_023E + B01001_047E +
              B01001_024E + B01001_048E +
              B01001_025E + B01001_049E,
            totalpopp = 1,
            age0004p =  age0004/totalpopulation,
            age0517p =  age0517/totalpopulation,
            age1844p =  age1844/totalpopulation,
            age4564p =  age4564/totalpopulation,
            age65p =  age65/totalpopulation) %>%
    select(GEOID, NAME, YEAR, totalpopulation:age65p)

# If there is more than o county, produce a summary record for the agency
    if(length(listID$list1) > 1) {  
    f.agesum_agy = f.AgeCTY %>%
            summarize( totalpopulation = 	sum(totalpopulation),
                        age0004 = sum(age0004),
                        age0517 = sum(age0517),
                        age1844 = sum(age1844),
                        age4564 = sum(age4564),
                        age65 = 	sum(age65)) %>%
             mutate(GEOID = "00000",
                    NAME = listID$plName1,
                    YEAR = curYr,
                    totalpopp = 1,
                    age0004p = age0004/totalpopulation,
                    age0517p = age0517/totalpopulation,
                    age1844p = age1844/totalpopulation,
                    age4564p = age4564/totalpopulation,
                    age65p =   age65/totalpopulation) %>%
           select(GEOID, NAME, YEAR, totalpopulation:age65, totalpopp : age65p)
   
  # Combine file
    f.agesum <- bind_rows(f.AgeUS, f.AgeST,  f.agesum_agy, f.AgeCTY)
 } else {
   f.agesum <- bind_rows(f.AgeUS, f.AgeST, f.AgeCTY)
 }
   
   # Wide to long

   f.place_tot <- f.agesum[,c(1:9)] %>%
            gather(age_cat, cat_pop, totalpopulation:age65, factor_key=TRUE)
   f.place_tot$age_cat <- as.character(f.place_tot$age_cat)
  
   f.place_pct <- f.agesum[,c(1:3,10:15)] %>%
            gather(age_cat, age_pct, totalpopp:age65p, factor_key=TRUE) 
   f.place_pct$age_cat <- as.character(f.place_pct$age_cat)
   
   f.place <- bind_cols(f.place_tot,f.place_pct)        
   f.place <- f.place[,c(1:4,7)] 
   names(f.place) <- c("geoname","county","age_cat","cat_pop","age_pct")
    
    #Fixing labels
 
    f.place$age_cat <- sub("totalpopulation","Total", f.place$age_cat)
    f.place$age_cat <- sub("age","",f.place$age_cat)
    f.place$age_cat <- paste0(substr(f.place$age_cat,1,2)," to ",substr(f.place$age_cat,3,4))
    f.place$age_cat <- ifelse(f.place$age_cat == "To to ta","Total",f.place$age_cat)
    f.place$age_cat <- ifelse(f.place$age_cat == "65 to ","65+",f.place$age_cat)
   
  # Fixing Formats for output data set
    f.place_dat <- f.place[order(f.place$county,f.place$age_cat),]
    f.place_dat$cat_pop <- format(round(as.numeric(f.place_dat$cat_pop), 0), big.mark=",")
    f.place_dat$age_pct <- percent(f.place_dat$age_pct * 100)
    
    names(f.place_dat) <- c("County","CountyCode", "Age Category","Count","Percentage")
   
    
     # prparing data for table   
    
    f.place_tab <- f.place
    f.place_tab$cat_pop <- format(round(f.place_tab$cat_pop,digits=0),  big.mark=",")
    f.place_tab$age_pct <- percent(f.place_tab$age_pct * 100)

    f.place_pct <-  f.place_tab[,c(1:3,5)] %>% spread(age_cat,age_pct)
    f.place_pct$type <- "Percentage"
    
    f.place_pop <- f.place_tab[,c(1:4)] %>%
           spread(age_cat,cat_pop)
    f.place_pop$type <- "Count"
    
 
    f.place_tab <- bind_rows(f.place_pct,f.place_pop)
    f.place_tab <- f.place_tab %>% arrange(county,desc(type))
    f.place_tab <- f.place_tab[,c(1,8,3:7)]
 
    names(f.place_tab)[1] <- "County"
      names(f.place_tab)[2] <- "Value"
    f.place_tab$County <- ifelse(f.place_tab$Value == "Count", "",f.place_tab$County)
    
    names(f.place_tab)[1] <- "Agency/County" 
    
  #Preparing Plot
  f.place <- f.place[which(f.place$age_cat != "Total"),] %>% arrange(county)
  f.place$age_pct <- round(f.place$age_pct, digits = 3)
  f.place$indText  <- paste0(f.place$geoname," Age Category: ",f.place$age_cat," Percentage: ",percent(f.place$age_pct * 100)," Count: ",NumFmt(f.place$cat_pop))  
  grTitle <- paste0("Population by Age, ",listID$plName1)
  xAxis <- list(title='Age Category')
  yAxis <- list(title = 'Percent',tickformat = ".1%")
  txtNames <- unique(f.place$geoname)

if(length(ctyfips) > 1 ){
 AgePlot <- f.place %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~age_pct,
  #  color=~cname,
    text = ~indText, textposition = "none",
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.place$geoname)[1]
      )
  )) %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4), 
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = genDropdown(txtNames)
        )))
} else {
   AgePlot <- f.place %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~age_pct,
  #  color=~cname,
    text = ~indText, textposition = "none",
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.place$geoname)[1]
      )
  ))   %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
}
 
 #Producing Flextable

   #Clearing geoname
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }

f.place_tab <- clrGeoname( f.place_tab,"Agency/County",npanel1,2)

 tab_head <- paste0("Population by Age, ",listID$plName1)

 
 f.flexage <- flextable(
       f.place_tab,
       col_keys = names(f.place_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=7) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=7) %>%
       align(j=1:7, align="center",part="header") %>%
       align(j=1:2, align="left", part="body") %>%
       align(j=3:7, align="right",part="body") %>%
       width(j= 1, width=3) %>%
       width(j=2:7,width=1) %>%
       height(part="footer", height=0.4)
 
 

  
  
  outList <- list("plot" = AgePlot, "data" = f.place, "table" = f.place_tab, "FlexTable" = f.flexage,"caption" = outCap)
  return(outList)
}