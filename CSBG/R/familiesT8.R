#'familiesT8 Outputs Tables and plots for families by FPL and headship
#'
#'    pulls data from ACS API 
#'
#'    This table does not report MOEs for ACS series, because of the lack of cunsus MOEs...
#'
#' @param listID the list containing place id and Place names
#' @param state is the state that the original fips
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @return kable formatted  table and data file
#' @export
#'
familiesT8 <- function(lvl,listID, ACS,curYr) {
  # Collecting place ids from  idList, setting default values


    ctyfips <- as.character(as.numeric(substr(listID$list1,3,5)))

  # Extracting data from tidycensus 
  f.ctyFAM <- codemog_api(data="b17010",db=ACS,sumlev="50",geography="sumlev",meta="no")
   
   
   
   f.ctyFAM[,c(3,8:48)] <- sapply(f.ctyFAM[,c(3,8:48)],as.numeric)

   f.ctyFAM_cty <- f.ctyFAM %>%
             filter(county %in% ctyfips) %>%  
            group_by(county) %>%
           mutate(
             POV.ALL.ALL = 	b17010002,
             POV.ALL.KIDS = 	b17010004 + b17010011 + b17010017,
             POV.ALL.NKIDS =	b17010008 + b17010015 + b17010021,
             POV.MAR.ALL = 	b17010003,
             POV.MAR.KIDS = 	b17010004,
             POV.MAR.NKIDS = 	b17010008,
             POV.MALE.ALL = 	b17010010,
             POV.MALE.KIDS = 	b17010011,
             POV.MALE.NKIDS = 	b17010015,
             POV.FEMALE.ALL = 	b17010016,
             POV.FEMALE.KIDS = 	b17010017,
             POV.FEMALE.NKIDS =	b17010021,
             NPOV.ALL.ALL = 	b17010022,
             NPOV.ALL.KIDS = 	b17010024 + b17010031 + b17010037,
             NPOV.ALL.NKIDS = b17010028 + b17010035 + b17010041,
             NPOV.MAR.ALL = 	b17010023,
             NPOV.MAR.KIDS = 	b17010024,
             NPOV.MAR.NKIDS = 	b17010028,
             NPOV.MALE.ALL =	b17010030,
             NPOV.MALE.KIDS =	b17010031,
             NPOV.MALE.NKIDS =	b17010035,
             NPOV.FEMALE.ALL =	b17010036,
             NPOV.FEMALE.KIDS = 	b17010037,
             NPOV.FEMALE.NKIDS = 	b17010041,
             TOT.ALL.ALL =	POV.ALL.ALL + NPOV.ALL.ALL,
             TOT.ALL.KIDS =	POV.ALL.KIDS + NPOV.ALL.KIDS,
             TOT.ALL.NKIDS = 	POV.ALL.NKIDS + NPOV.ALL.NKIDS,
             TOT.MAR.ALL =	POV.MAR.ALL + NPOV.MAR.ALL,
             TOT.MAR.KIDS =	POV.MAR.KIDS + NPOV.MAR.KIDS,
             TOT.MAR.NKIDS =	POV.MAR.NKIDS + NPOV.MAR.NKIDS,
             TOT.MALE.ALL =	POV.MALE.ALL + NPOV.MALE.ALL,
             TOT.MALE.KIDS =	POV.MALE.KIDS + NPOV.MALE.KIDS,
             TOT.MALE.NKIDS =	POV.MALE.NKIDS + NPOV.MALE.NKIDS,
             TOT.FEMALE.ALL =	POV.FEMALE.ALL + NPOV.FEMALE.ALL,
             TOT.FEMALE.KIDS =	POV.FEMALE.KIDS + NPOV.FEMALE.KIDS,
             TOT.FEMALE.NKIDS =	POV.FEMALE.NKIDS + NPOV.FEMALE.NKIDS,
             POV.ALL.ALL.PCT =	POV.ALL.ALL/POV.ALL.ALL,
             POV.ALL.KIDS.PCT = 	POV.ALL.KIDS/POV.ALL.ALL,
             POV.ALL.NKIDS.PCT = 	POV.ALL.NKIDS/POV.ALL.ALL,
             POV.MAR.ALL.PCT =	POV.MAR.ALL/POV.MAR.ALL,
             POV.MAR.KIDS.PCT =	POV.MAR.KIDS/POV.MAR.ALL,
             POV.MAR.NKIDS.PCT =	POV.MAR.NKIDS/POV.MAR.ALL,
             POV.MALE.ALL.PCT =	POV.MALE.ALL/POV.MALE.ALL,
             POV.MALE.KIDS.PCT =	POV.MALE.KIDS/POV.MALE.ALL,
             POV.MALE.NKIDS.PCT =	POV.MALE.NKIDS/POV.MALE.ALL,
             POV.FEMALE.ALL.PCT =	POV.FEMALE.ALL/POV.FEMALE.ALL,
             POV.FEMALE.KIDS.PCT =	POV.FEMALE.KIDS/POV.FEMALE.ALL,
             POV.FEMALE.NKIDS.PCT =	POV.FEMALE.NKIDS/POV.FEMALE.ALL,
             NPOV.ALL.ALL.PCT =	NPOV.ALL.ALL/NPOV.ALL.ALL,
             NPOV.ALL.KIDS.PCT =	NPOV.ALL.KIDS/NPOV.ALL.ALL,
             NPOV.ALL.NKIDS.PCT =	NPOV.ALL.NKIDS/NPOV.ALL.ALL,
             NPOV.MAR.ALL.PCT =	NPOV.MAR.ALL/NPOV.MAR.ALL,
             NPOV.MAR.KIDS.PCT =	NPOV.MAR.KIDS/NPOV.MAR.ALL,
             NPOV.MAR.NKIDS.PCT =	NPOV.MAR.NKIDS/NPOV.MAR.ALL,
             NPOV.MALE.ALL.PCT =	NPOV.MALE.ALL/NPOV.MALE.ALL,
             NPOV.MALE.KIDS.PCT =	NPOV.MALE.KIDS/NPOV.MALE.ALL,
             NPOV.MALE.NKIDS.PCT =	NPOV.MALE.NKIDS/NPOV.MALE.ALL,
             NPOV.FEMALE.ALL.PCT =	NPOV.FEMALE.ALL/NPOV.FEMALE.ALL,
             NPOV.FEMALE.KIDS.PCT =	NPOV.FEMALE.KIDS/NPOV.FEMALE.ALL,
             NPOV.FEMALE.NKIDS.PCT =	NPOV.FEMALE.NKIDS/NPOV.FEMALE.ALL,
             TOT.ALL.ALL.PCT =	TOT.ALL.ALL/TOT.ALL.ALL,
             TOT.ALL.KIDS.PCT =	TOT.ALL.KIDS/TOT.ALL.ALL,
             TOT.ALL.NKIDS.PCT =	TOT.ALL.NKIDS/TOT.ALL.ALL,
             TOT.MAR.ALL.PCT =	TOT.MAR.ALL/TOT.MAR.ALL,
             TOT.MAR.KIDS.PCT =	TOT.MAR.KIDS/TOT.MAR.ALL,
             TOT.MAR.NKIDS.PCT =	TOT.MAR.NKIDS/TOT.MAR.ALL,
             TOT.MALE.ALL.PCT =	TOT.MALE.ALL/TOT.MALE.ALL,
             TOT.MALE.KIDS.PCT =	TOT.MALE.KIDS/TOT.MALE.ALL,
             TOT.MALE.NKIDS.PCT =	TOT.MALE.NKIDS/TOT.MALE.ALL,
             TOT.FEMALE.ALL.PCT =	TOT.FEMALE.ALL/TOT.FEMALE.ALL,
             TOT.FEMALE.KIDS.PCT =	TOT.FEMALE.KIDS/TOT.FEMALE.ALL,
             TOT.FEMALE.NKIDS.PCT =	TOT.FEMALE.NKIDS/TOT.FEMALE.ALL
           )

	
   f.ctyFAM_cty <- f.ctyFAM_cty[,c("geoname",	"county",				
                                   "POV.ALL.ALL", "POV.ALL.KIDS", "POV.ALL.NKIDS",
                                   "POV.MAR.ALL", "POV.MAR.KIDS", "POV.MAR.NKIDS", 
                                   "POV.MALE.ALL", "POV.MALE.KIDS", "POV.MALE.NKIDS", 
                                   "POV.FEMALE.ALL", "POV.FEMALE.KIDS", "POV.FEMALE.NKIDS", 
                                   "NPOV.ALL.ALL", "NPOV.ALL.KIDS", "NPOV.ALL.NKIDS",
                                   "NPOV.MAR.ALL", "NPOV.MAR.KIDS", "NPOV.MAR.NKIDS", 
                                   "NPOV.MALE.ALL", "NPOV.MALE.KIDS", "NPOV.MALE.NKIDS", 
                                   "NPOV.FEMALE.ALL", "NPOV.FEMALE.KIDS", "NPOV.FEMALE.NKIDS", 
                                   "TOT.ALL.ALL", "TOT.ALL.KIDS", "TOT.ALL.NKIDS", 
                                   "TOT.MAR.ALL", "TOT.MAR.KIDS", "TOT.MAR.NKIDS", 
                                   "TOT.MALE.ALL", "TOT.MALE.KIDS", "TOT.MALE.NKIDS", 
                                   "TOT.FEMALE.ALL", "TOT.FEMALE.KIDS", "TOT.FEMALE.NKIDS",
                                   "POV.ALL.ALL.PCT", "POV.ALL.KIDS.PCT", "POV.ALL.NKIDS.PCT",
                                   "POV.MAR.ALL.PCT", "POV.MAR.KIDS.PCT", "POV.MAR.NKIDS.PCT", 
                                   "POV.MALE.ALL.PCT", "POV.MALE.KIDS.PCT", "POV.MALE.NKIDS.PCT", 
                                   "POV.FEMALE.ALL.PCT", "POV.FEMALE.KIDS.PCT", "POV.FEMALE.NKIDS.PCT",
                                   "NPOV.ALL.ALL.PCT", "NPOV.ALL.KIDS.PCT", "NPOV.ALL.NKIDS.PCT",
                                   "NPOV.MAR.ALL.PCT", "NPOV.MAR.KIDS.PCT", "NPOV.MAR.NKIDS.PCT", 
                                   "NPOV.MALE.ALL.PCT", "NPOV.MALE.KIDS.PCT", "NPOV.MALE.NKIDS.PCT", 
                                   "NPOV.FEMALE.ALL.PCT", "NPOV.FEMALE.KIDS.PCT", "NPOV.FEMALE.NKIDS.PCT", 
                                   "TOT.ALL.ALL.PCT", "TOT.ALL.KIDS.PCT", "TOT.ALL.NKIDS.PCT",
                                   "TOT.MAR.ALL.PCT", "TOT.MAR.KIDS.PCT", "TOT.MAR.NKIDS.PCT", 
                                   "TOT.MALE.ALL.PCT", "TOT.MALE.KIDS.PCT", "TOT.MALE.NKIDS.PCT", 
                                   "TOT.FEMALE.ALL.PCT", "TOT.FEMALE.KIDS.PCT", "TOT.FEMALE.NKIDS.PCT"
                                   )]
 
if(length(ctyfips) > 1) {
     f.ctyFAM_agy <- f.ctyFAM %>%
             filter(county %in% ctyfips) %>%
          summarize(  
            POV.ALL.ALL = sum(b17010002),
            POV.ALL.KIDS =  sum(b17010004, b17010011, b17010017),
            POV.ALL.NKIDS = sum(b17010008, b17010015, b17010021),
            POV.MAR.ALL = sum(b17010003),
            POV.MAR.KIDS = sum(b17010004),
            POV.MAR.NKIDS =	 sum(b17010008),
            POV.MALE.ALL = sum(b17010010),
            POV.MALE.KIDS =	 sum(b17010011),
            POV.MALE.NKIDS = sum(b17010015),
            POV.FEMALE.ALL = sum(b17010016),
            POV.FEMALE.KIDS	= sum(b17010017),
            POV.FEMALE.NKIDS = sum(b17010021),
            NPOV.ALL.ALL = sum(b17010022),
            NPOV.ALL.KIDS = sum(b17010024, b17010031, b17010037),
            NPOV.ALL.NKIDS = sum(b17010028, b17010035, b17010041),
            NPOV.MAR.ALL = sum(b17010023),
            NPOV.MAR.KIDS =	sum(b17010024),
            NPOV.MAR.NKIDS = sum(b17010028),
            NPOV.MALE.ALL =	sum(b17010030),
            NPOV.MALE.KIDS = sum(b17010031),
            NPOV.MALE.NKIDS = sum(b17010035),
            NPOV.FEMALE.ALL = sum(b17010036),
            NPOV.FEMALE.KIDS = sum(b17010037),
            NPOV.FEMALE.NKIDS =	sum(b17010041)
          ) %>%
         mutate(
           TOT.ALL.ALL =	POV.ALL.ALL + NPOV.ALL.ALL,
           TOT.ALL.KIDS =	POV.ALL.KIDS + NPOV.ALL.KIDS,
           TOT.ALL.NKIDS = 	POV.ALL.NKIDS + NPOV.ALL.NKIDS,
           TOT.MAR.ALL =	POV.MAR.ALL + NPOV.MAR.ALL,
           TOT.MAR.KIDS =	POV.MAR.KIDS + NPOV.MAR.KIDS,
           TOT.MAR.NKIDS =	POV.MAR.NKIDS + NPOV.MAR.NKIDS,
           TOT.MALE.ALL =	POV.MALE.ALL + NPOV.MALE.ALL,
           TOT.MALE.KIDS =	POV.MALE.KIDS + NPOV.MALE.KIDS,
           TOT.MALE.NKIDS =	POV.MALE.NKIDS + NPOV.MALE.NKIDS,
           TOT.FEMALE.ALL =	POV.FEMALE.ALL + NPOV.FEMALE.ALL,
           TOT.FEMALE.KIDS =	POV.FEMALE.KIDS + NPOV.FEMALE.KIDS,
           TOT.FEMALE.NKIDS =	POV.FEMALE.NKIDS + NPOV.FEMALE.NKIDS,
           POV.ALL.ALL.PCT =	POV.ALL.ALL/POV.ALL.ALL,
           POV.ALL.KIDS.PCT = 	POV.ALL.KIDS/POV.ALL.ALL,
           POV.ALL.NKIDS.PCT = 	POV.ALL.NKIDS/POV.ALL.ALL,
           POV.MAR.ALL.PCT =	POV.MAR.ALL/POV.MAR.ALL,
           POV.MAR.KIDS.PCT =	POV.MAR.KIDS/POV.MAR.ALL,
           POV.MAR.NKIDS.PCT =	POV.MAR.NKIDS/POV.MAR.ALL,
           POV.MALE.ALL.PCT =	POV.MALE.ALL/POV.MALE.ALL,
           POV.MALE.KIDS.PCT =	POV.MALE.KIDS/POV.MALE.ALL,
           POV.MALE.NKIDS.PCT =	POV.MALE.NKIDS/POV.MALE.ALL,
           POV.FEMALE.ALL.PCT =	POV.FEMALE.ALL/POV.FEMALE.ALL,
           POV.FEMALE.KIDS.PCT =	POV.FEMALE.KIDS/POV.FEMALE.ALL,
           POV.FEMALE.NKIDS.PCT =	POV.FEMALE.NKIDS/POV.FEMALE.ALL,
           NPOV.ALL.ALL.PCT =	NPOV.ALL.ALL/NPOV.ALL.ALL,
           NPOV.ALL.KIDS.PCT =	NPOV.ALL.KIDS/NPOV.ALL.ALL,
           NPOV.ALL.NKIDS.PCT =	NPOV.ALL.NKIDS/NPOV.ALL.ALL,
           NPOV.MAR.ALL.PCT =	NPOV.MAR.ALL/NPOV.MAR.ALL,
           NPOV.MAR.KIDS.PCT =	NPOV.MAR.KIDS/NPOV.MAR.ALL,
           NPOV.MAR.NKIDS.PCT =	NPOV.MAR.NKIDS/NPOV.MAR.ALL,
           NPOV.MALE.ALL.PCT =	NPOV.MALE.ALL/NPOV.MALE.ALL,
           NPOV.MALE.KIDS.PCT =	NPOV.MALE.KIDS/NPOV.MALE.ALL,
           NPOV.MALE.NKIDS.PCT =	NPOV.MALE.NKIDS/NPOV.MALE.ALL,
           NPOV.FEMALE.ALL.PCT =	NPOV.FEMALE.ALL/NPOV.FEMALE.ALL,
           NPOV.FEMALE.KIDS.PCT =	NPOV.FEMALE.KIDS/NPOV.FEMALE.ALL,
           NPOV.FEMALE.NKIDS.PCT =	NPOV.FEMALE.NKIDS/NPOV.FEMALE.ALL,
           TOT.ALL.ALL.PCT =	TOT.ALL.ALL/TOT.ALL.ALL,
           TOT.ALL.KIDS.PCT =	TOT.ALL.KIDS/TOT.ALL.ALL,
           TOT.ALL.NKIDS.PCT =	TOT.ALL.NKIDS/TOT.ALL.ALL,
           TOT.MAR.ALL.PCT =	TOT.MAR.ALL/TOT.MAR.ALL,
           TOT.MAR.KIDS.PCT =	TOT.MAR.KIDS/TOT.MAR.ALL,
           TOT.MAR.NKIDS.PCT =	TOT.MAR.NKIDS/TOT.MAR.ALL,
           TOT.MALE.ALL.PCT =	TOT.MALE.ALL/TOT.MALE.ALL,
           TOT.MALE.KIDS.PCT =	TOT.MALE.KIDS/TOT.MALE.ALL,
           TOT.MALE.NKIDS.PCT =	TOT.MALE.NKIDS/TOT.MALE.ALL,
           TOT.FEMALE.ALL.PCT =	TOT.FEMALE.ALL/TOT.FEMALE.ALL,
           TOT.FEMALE.KIDS.PCT =	TOT.FEMALE.KIDS/TOT.FEMALE.ALL,
           TOT.FEMALE.NKIDS.PCT =	TOT.FEMALE.NKIDS/TOT.FEMALE.ALL
         )
    f.ctyFAM_agy$geoname <- listID$plName1
    f.ctyFAM_agy$county <- 0

    f.ctyFAM_agy <- f.ctyFAM_agy[,c("geoname",	"county",				
                                    "POV.ALL.ALL", "POV.ALL.KIDS", "POV.ALL.NKIDS",
                                    "POV.MAR.ALL", "POV.MAR.KIDS", "POV.MAR.NKIDS", 
                                    "POV.MALE.ALL", "POV.MALE.KIDS", "POV.MALE.NKIDS", 
                                    "POV.FEMALE.ALL", "POV.FEMALE.KIDS", "POV.FEMALE.NKIDS", 
                                    "NPOV.ALL.ALL", "NPOV.ALL.KIDS", "NPOV.ALL.NKIDS",
                                    "NPOV.MAR.ALL", "NPOV.MAR.KIDS", "NPOV.MAR.NKIDS", 
                                    "NPOV.MALE.ALL", "NPOV.MALE.KIDS", "NPOV.MALE.NKIDS", 
                                    "NPOV.FEMALE.ALL", "NPOV.FEMALE.KIDS", "NPOV.FEMALE.NKIDS", 
                                    "TOT.ALL.ALL", "TOT.ALL.KIDS", "TOT.ALL.NKIDS", 
                                    "TOT.MAR.ALL", "TOT.MAR.KIDS", "TOT.MAR.NKIDS", 
                                    "TOT.MALE.ALL", "TOT.MALE.KIDS", "TOT.MALE.NKIDS", 
                                    "TOT.FEMALE.ALL", "TOT.FEMALE.KIDS", "TOT.FEMALE.NKIDS",
                                    "POV.ALL.ALL.PCT", "POV.ALL.KIDS.PCT", "POV.ALL.NKIDS.PCT",
                                    "POV.MAR.ALL.PCT", "POV.MAR.KIDS.PCT", "POV.MAR.NKIDS.PCT", 
                                    "POV.MALE.ALL.PCT", "POV.MALE.KIDS.PCT", "POV.MALE.NKIDS.PCT", 
                                    "POV.FEMALE.ALL.PCT", "POV.FEMALE.KIDS.PCT", "POV.FEMALE.NKIDS.PCT",
                                    "NPOV.ALL.ALL.PCT", "NPOV.ALL.KIDS.PCT", "NPOV.ALL.NKIDS.PCT",
                                    "NPOV.MAR.ALL.PCT", "NPOV.MAR.KIDS.PCT", "NPOV.MAR.NKIDS.PCT", 
                                    "NPOV.MALE.ALL.PCT", "NPOV.MALE.KIDS.PCT", "NPOV.MALE.NKIDS.PCT", 
                                    "NPOV.FEMALE.ALL.PCT", "NPOV.FEMALE.KIDS.PCT", "NPOV.FEMALE.NKIDS.PCT", 
                                    "TOT.ALL.ALL.PCT", "TOT.ALL.KIDS.PCT", "TOT.ALL.NKIDS.PCT",
                                    "TOT.MAR.ALL.PCT", "TOT.MAR.KIDS.PCT", "TOT.MAR.NKIDS.PCT", 
                                    "TOT.MALE.ALL.PCT", "TOT.MALE.KIDS.PCT", "TOT.MALE.NKIDS.PCT", 
                                    "TOT.FEMALE.ALL.PCT", "TOT.FEMALE.KIDS.PCT", "TOT.FEMALE.NKIDS.PCT"
                                   )]
    
   f.ctyFAM_cty <- bind_rows(f.ctyFAM_agy, f.ctyFAM_cty)
   
}
 f.ctyFAM_cty$geoname <- sub(", Colorado","",f.ctyFAM_cty$geoname)  

 ctyList <- as.list(unique(sort(f.ctyFAM_cty$county)))
 ctyName <- f.ctyFAM_cty[,c(1,2)]
 ctyName <- as.list(unique(ctyName[order(f.ctyFAM_cty$county),1]))
 
 # preparing files

     f.ctyFAM_cty[is.na(f.ctyFAM_cty)] <- 0
     f.ctyFAM_tot <- f.ctyFAM_cty[, c(1:38)]
     f.ctyFAM_pct <- f.ctyFAM_cty[,c(1, 2, 39:74)]
     
     f.ctyFAML_tot <- f.ctyFAM_tot %>% 
          gather(var, count, POV.ALL.ALL:TOT.FEMALE.NKIDS, factor_key=TRUE) %>%
          separate(var,c("pov","famtype","kids")) %>% arrange(famtype)
     
      f.ctyFAML_pct <- f.ctyFAM_pct %>% 
          gather(var, pct, POV.ALL.ALL.PCT:TOT.FEMALE.NKIDS.PCT, factor_key=TRUE) %>%
          separate(var,c("pov","famtype","kids",NA)) %>% arrange(famtype)

# revising pov
   f.ctyFAML_tot$pov <-plyr::revalue(f.ctyFAML_tot$pov, c("POV" = "Below Poverty Level",
                             "NPOV" = "Above Poverty Level",
                             "TOT" = "Total"))
    
  f.ctyFAML_pct$pov <-plyr::revalue(f.ctyFAML_pct$pov, c("POV" = "Below Poverty Level",
                             "NPOV" = "Above Poverty Level",
                             "TOT" = "Total"))      
    
# revising Fam Type
  
   f.ctyFAML_tot$famtype <-plyr::revalue(f.ctyFAML_tot$famtype, c("ALL" = "All Families",
                             "MAR" = "Married Couple",
                             "MALE" = "Male Householder",
                             "FEMALE" = "Female Householder"))
    
  f.ctyFAML_pct$famtype <-plyr::revalue(f.ctyFAML_pct$famtype, c("ALL" = "All Families",
                             "MAR" = "Married Couple",
                             "MALE" = "Male Householder",
                             "FEMALE" = "Female Householder"))
    

  # revising kids
   f.ctyFAML_tot$kids <-plyr::revalue(f.ctyFAML_tot$kids, c("KIDS" = "Children Present",
                             "NKIDS" = "No Children Present",
                             "ALL" = "All Families"))

  
   f.ctyFAML_pct$kids <-plyr::revalue(f.ctyFAML_pct$kids, c("KIDS" = "Children Present",
                             "NKIDS" = "No Children Present",
                             "ALL" = "All Families"))

  
                                                             

    # Plotly 

    f.ctyFAML_PLT <- f.ctyFAML_pct[which(f.ctyFAML_pct$pov == "Below Poverty Level" &
                                         f.ctyFAML_pct$kids != "All Families"),] %>% arrange(famtype,kids,county)
    f.ctyFAML_PLT$famtype <- factor(f.ctyFAML_PLT$famtype,c( "Female Householder",
                                                             "Male Householder",
                                                            "Married Couple", "All Families"))                                                           
    
    f.ctyFAML_PLT$indText  <- paste0( f.ctyFAML_PLT$geoname," Family Type: ", f.ctyFAML_PLT$famtype," ",percent(f.ctyFAML_PLT$pct * 100))  
    grTitle <- paste0("Table 8: Families by Type, Below FPL, ",listID$plName1)
    outCap <- captionSrc("ACS",ACS,"B17010") 
    xAxis <- list(title = "Family Type")
    yAxis <- list(title = 'Percent',tickformat = ".1%")
    
    
 
 
# % persons in poverty with Disabilities
if(length(ctyfips) > 1 ){
 FAMPLOT <- f.ctyFAML_PLT %>%
 plot_ly(
    type = 'bar', 
    x = ~famtype, 
    y = ~pct,
    color= ~kids,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyFAML_PLT$geoname)[1]
      )
  )) %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis, 
          showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4) ,
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyFAML_PLT$geoname)[1]),
               label = unique(f.ctyFAML_PLT$geoname)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyFAML_PLT$geoname)[2]),
               label = unique(f.ctyFAML_PLT$geoname)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyFAML_PLT$geoname)[3]),
               label = unique(f.ctyFAML_PLT$geoname)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyFAML_PLT$geoname)[4]),
               label = unique(f.ctyFAML_PLT$geoname)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyFAML_PLT$geoname)[5]),
               label = unique(f.ctyFAML_PLT$geoname)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyFAML_PLT$geoname)[6]),
               label = unique(f.ctyFAML_PLT$geoname)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyFAML_PLT$geoname)[7]),
               label = unique(f.ctyFAML_PLT$geoname)[7])
      )
  )))
} else {
   FAMPLOT <- f.ctyFAML_PLT %>%
  plot_ly(
    type = 'bar',
    x = ~famtype, 
    y = ~pct,
    color = ~kids,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyFAML_PLT$geoname)[1]
      )
  ))   %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
}    
    

  
    # flex Table and output data file
   
    povList <- c("Below Poverty Level", "Above Poverty Level",  "Total")
    
    famList <-  c("Female Householder", "Male Householder", "Married Couple", "All Families")
    kidsList <- c("Children Present", "No Children Present", "All Families")
    
                       
    f.ctyFAML_tot$count <- format(round(f.ctyFAML_tot$count ,digits=0),  big.mark=",")
    f.ctyFAML_pct$pct <- percent(f.ctyFAML_pct$pct * 100)
    
     f.ctyFAML_tot$type2 <- "Count"
     f.ctyFAML_pct$type2 <- "Percentage"
    
    f.ctyFAM_Count <-  f.ctyFAML_tot %>% spread(kids,count)
    f.ctyFAM_Percent <-  f.ctyFAML_pct %>% spread(kids,pct)
   
    f.ctyFAM_tab <- bind_rows(f.ctyFAM_Count,f.ctyFAM_Percent)  
    
    
    # reordering Records for Table

    f.ctyFAM_tab  <- f.ctyFAM_tab %>% arrange(factor(county, levels = ctyList),  
                                              factor(famtype, levels = famList), 
                                              factor(pov, levels = povList),
                                              desc(type2))
    
    
  
    #Clearing geoname
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
    
    f.ctyFAM_tab <- clrGeoname(f.ctyFAM_tab,"geoname",npanel1,24)
    f.ctyFAM_tab <- clrGeoname(f.ctyFAM_tab,"famtype",npanel1,6)
    for(i in 1:nrow(f.ctyFAM_tab)){
      if(i %% 2 == 0){
        f.ctyFAM_tab[i,3] <- ""
      } 
    }

    
     #Producing Flextable
 
 tab_head <- paste0("Table 8: Families by Type and Poverty Status, ",listID$plName1)

 f.ctyFAM_tab <-  f.ctyFAM_tab[,c(1,4,3,5,7,8,6)]
 names(f.ctyFAM_tab) <- c("Agency/County","Family Type","Poverty Level","Value","Children Present","No Children Present","All Families")

   
   f.flexDIS <- flextable(
       f.ctyFAM_tab,
       col_keys = names(f.ctyFAM_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=7) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=7) %>%
       align(j=1:2, align="left", part="body") 
 

  outList <- list("plot" = FAMPLOT, "FlexTable" = f.flexDIS, "data" = f.ctyFAM_tab,"caption" = outCap)
  return(outList)
}


