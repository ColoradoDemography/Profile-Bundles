#' agePlotPRO Creates a Chart comparing The age distribution of a selected counties for a simgle year
#'
#' @param DBPool the active database pool
#' @param lvl is the data type ("Regional Summary" or "Counties)
#' @param listID is the list of selected county codes
#' @param curYr is the single year value to be extracted by county_sya
#' @return plotly graphic, data table and data file
#' @export

agePlotPRO  <- function(lvl,listID,ACS,curYr) {

  # Collecting List of Counties

  f.ctyAge <- codemog_api(data="b01001",db=ACS,sumlev="50",geography="sumlev",meta="no")
  f.ctyAge[,c(3,8:56)] <- sapply(f.ctyAge[,c(3,8:56)],as.numeric)
 
  ctyfips <- as.character(as.numeric(substr(listID$list1,3,5)))
   outCap <- captionSrc("ACS",ACS,"B01001")

# Creating County level data
   f.agesum_cty = f.ctyAge %>%
             filter(county %in% ctyfips) %>%
            group_by(county) %>%
            mutate( totalpopulation = 	b01001002 + b01001026		,
                    age0004 = 	b01001003 + b01001027		,
                    age0517 = 	b01001004 + b01001028		 +
                    	b01001005 + b01001029		 +
                    	b01001006 + b01001030		,
                    age1864 = 	b01001007 + b01001031		 +
                    	b01001008 + b01001032		 +
                    	b01001009 + b01001033		 +
                    	b01001010 + b01001034		 +
                    	b01001011 + b01001035		 +
                    	b01001012 + b01001036		 +
                    	b01001013 + b01001037		 +
                    	b01001014 + b01001038		 +
                    	b01001015 + b01001039		 +
                    	b01001016 + b01001040		 +
                    	b01001017 + b01001041		 +
                    	b01001018 + b01001042		 +
                    	b01001019 + b01001043		,
                    age65 = 	b01001020 + b01001044		 +
                    	b01001021 + b01001045		 +
                    	b01001022 + b01001046		 +
                    	b01001023 + b01001047		 +
                    	b01001024 + b01001048		 +
                    	b01001025 + b01001049		 ,
                    totalpopp = 1,
                    age0004p =  age0004/totalpopulation,
                    age0517p =  age0517/totalpopulation,
                    age1864p =  age1864/totalpopulation,
                    age65p =  age65/totalpopulation)
                    
   
   f.agesum_cty$geoname <- sub(", Colorado","",f.agesum_cty$geoname)
   f.agesum_cty <- f.agesum_cty[,c(1,3,57:66)]
   
# If there is more than o county, produce a summary record for the agency
   
    if(length(ctyfips) > 1) {  

    f.agesum_agy = f.ctyAge %>%
             filter(county %in% ctyfips) %>%
            summarize( totalpopulation = 	sum(	b01001002 + b01001026		),
                        age0004 = 	sum(	b01001003 + b01001027		),
                        age0517 = 	sum(	b01001004 + b01001028		 +
                        		b01001005 + b01001029		 +
                        		b01001006 + b01001030		),
                        age1864 = 	sum(	b01001007 + b01001031		 +
                        		b01001008 + b01001032		 +
                        		b01001009 + b01001033		 +
                        		b01001010 + b01001034		 +
                        		b01001011 + b01001035		 +
                        		b01001012 + b01001036		 +
                        		b01001013 + b01001037		 +
                        		b01001014 + b01001038		 +
                        		b01001015 + b01001039		 +
                        		b01001016 + b01001040		 +
                        		b01001017 + b01001041		 +
                        		b01001018 + b01001042		 +
                        		b01001019 + b01001043		),
                        age65 = 	sum(	b01001020 + b01001044		 +
                        		b01001021 + b01001045		 +
                        		b01001022 + b01001046		 +
                        		b01001023 + b01001047		 +
                        		b01001024 + b01001048		 +
                        		b01001025 + b01001049		)) %>%
             mutate(totalpopp = 1,
                    age0004p =  age0004/totalpopulation,
                    age0517p =  age0517/totalpopulation,
                    age1864p =  age1864/totalpopulation,
                    age65p =  age65/totalpopulation)  
                 
    f.agesum_agy$geoname <- listID$plName1
    f.agesum_agy$county <- 0
    f.agesum_agy <- f.agesum_agy[,c(11,12,1:10)]
   
  # Combine file
    f.agesum <- bind_rows(f.agesum_agy,f.agesum_cty)
 } else {
   f.agesum <- f.agesum_cty
 }
    
   # Wide to long

   f.place_tot <- f.agesum[,c(1:7)] %>%
            gather(age_cat, cat_pop, totalpopulation:age65, factor_key=TRUE)
   f.place_tot$age_cat <- as.character(f.place_tot$age_cat)
  
   f.place_pct <- f.agesum[,c(2,8:12)] %>%
            gather(age_cat, age_pct, totalpopp:age65p, factor_key=TRUE) 
   f.place_pct$age_cat <- as.character(f.place_pct$age_cat)
   
   f.place <- bind_cols(f.place_tot,f.place_pct)        
   f.place <- f.place[,c(1:4,7)] 
    
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
      names(f.place_tab)[2] <- "Type"
    f.place_tab$County <- ifelse(f.place_tab$Type == "Count", "",f.place_tab$County)
   
    

  #Preparing Plot
  f.place <- f.place[which(f.place$age_cat != "Total"),] %>% arrange(county)
  f.place$indText  <- paste0(f.place$geoname," Age Category: ",f.place$age_cat," Percentage: ",percent(f.place$age_pct * 100)," Count: ",NumFmt(f.place$cat_pop))  
  grTitle <- paste0("Population by Age, ",listID$plName1)
  xAxis <- list(title='Age Category')
  yAxis <- list(title = 'Percent',tickformat = ".1%")

if(length(ctyfips) > 1 ){
 AgePlot <- f.place %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~age_pct,
  #  color=~cname,
    text = ~indText,
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
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.place$geoname)[1]),
               label = unique(f.place$geoname)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.place$geoname)[2]),
               label = unique(f.place$geoname)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.place$geoname)[3]),
               label = unique(f.place$geoname)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.place$geoname)[4]),
               label = unique(f.place$geoname)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.place$geoname)[5]),
               label = unique(f.place$geoname)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.place$geoname)[6]),
               label = unique(f.place$geoname)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.place$geoname)[7]),
               label = unique(f.place$geoname)[7])
      )
  )))
} else {
   AgePlot <- f.place %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~age_pct,
  #  color=~cname,
    text = ~indText,
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
 
f.place_tab <- clrGeoname( f.place_tab,"County",npanel1,2)
 
 tab_head <- paste0("Population by age, ",listID$plName1)

 
 f.flexage <- flextable(
       f.place_tab,
       col_keys = names(f.place_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=7) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=7) %>%
       align(j=1:2, align="left", part="body") 
 

  
  
  outList <- list("plot" = AgePlot, "data" = f.place_dat, "table" = f.place_tab, "FlexTable" = f.flexage,"caption" = outCap)
  return(outList)
}