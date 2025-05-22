#' povertyPRO Creates a materials showing ppopulation by federal poverty level
#'
# 'Produces plotly plot, data and table for selected CSBG agencies 10/2019
#'
#' @param lvl is the data type ("Regional Summary" or "Counties)
#' @param listID is the list of selected county codes
#' @param ACS  Specify the vintage of the ACS 5-year file 
#' @return plotly graphic, data table and data file
#' @export
povertyPRO <- function(listID, curYr,inData, barCol, barCol2){

  # Collecting List of Counties
   outCap <- captionSrc("ACS",curYr,"C17002") 
   cty_str <- listID$list2
   full_str <- c("01000","08000",unlist(cty_str))
   cty_str2 <- c("08000",unlist(cty_str))
   cty_name <- CountyName(cty_str)
   cty_num <- as.numeric(str_sub(cty_str,3,5))
   name1 <- listID$plName1

   f.C17002_ACS <- inData[["C17002"]] %>% filter(GEOID %in% full_str) %>%
     mutate(
       TOT_EST = .[[3]],	
       TOT_MOE = .[[4]]^2,
       POV_LT100_EST = .[[5]] + .[[7]],	
       POV_LT100_MOE = .[[6]]^2 + .[[8]]^2,
       POV_LT125_EST = .[[9]],	
       POV_LT125_MOE = .[[10]]^2,
       POV_LT200_EST = .[[11]] + .[[13]] + .[[15]],	
       POV_LT200_MOE = .[[12]]^2 + .[[14]]^2 + .[[16]]^2,
       POV_GE200_EST = .[[17]],	
       POV_GE200_MOE = .[[18]]^2
     )  %>%
     select(GEOID, NAME, TOT_EST : POV_GE200_MOE)
   
  

   
   if(length(cty_str) > 1) {  #Summarize for Multi County Place
     f.C17002_csbg <- f.C17002_ACS %>% filter(GEOID %in% cty_str) %>%
       summarise(TOT_EST =sum(TOT_EST, na.rm=TRUE),	
                 TOT_MOE =sum(TOT_MOE, na.rm=TRUE),
                 POV_LT100_EST =sum(POV_LT100_EST, na.rm=TRUE),	
                 POV_LT100_MOE =sum(POV_LT100_MOE, na.rm=TRUE),
                 POV_LT125_EST =sum(POV_LT125_EST, na.rm=TRUE),	
                 POV_LT125_MOE =sum(POV_LT125_MOE, na.rm=TRUE),
                 POV_LT200_EST =sum(POV_LT200_EST, na.rm=TRUE),	
                 POV_LT200_MOE =sum(POV_LT200_MOE, na.rm=TRUE),
                 POV_GE200_EST =sum(POV_GE200_EST, na.rm=TRUE),	
                 POV_GE200_MOE =sum(POV_GE200_MOE, na.rm=TRUE)) %>%
       mutate(GEOID = "080.5",
              NAME = name1) %>%
       select(GEOID, NAME, TOT_EST :POV_GE200_MOE)
     
     f.C17002_US <- f.C17002_ACS %>% filter(GEOID == "01000")
     f.C17002_CO <- f.C17002_ACS %>% filter(GEOID == "08000")
     f.C17002_CTY <- f.C17002_ACS %>% filter(GEOID %in% cty_str)
     
     f.C17002_FIN <- bind_rows(f.C17002_US, f.C17002_CO, f.C17002_csbg, f.C17002_CTY)
   } else {
     f.C17002_FIN <- f.C17002_ACS %>% filter(GEOID %in% full_str)
   }
 
   f.C17002_FIN <- f.C17002_FIN %>%
          mutate(NAME = str_replace(NAME, ", Colorado",""),
                 TOT_MOE = sqrt(TOT_MOE),
                 POV_LT100_MOE = sqrt(POV_LT100_MOE),
                 POV_LT125_MOE = sqrt(POV_LT125_MOE),
                 POV_LT200_MOE = sqrt(POV_LT200_MOE),
                 POV_GE200_MOE = sqrt(POV_GE200_MOE),
                 POV_LT100_EST_PCT = POV_LT100_EST/TOT_EST,
                 POV_LT100_MOE_PCT = pctMOE(TOT_EST,TOT_MOE,POV_LT100_MOE,POV_LT100_EST_PCT),
                 POV_LT125_EST_PCT = POV_LT125_EST/TOT_EST,
                 POV_LT125_MOE_PCT = pctMOE(TOT_EST,TOT_MOE,POV_LT125_MOE,POV_LT125_EST_PCT),
                 POV_LT200_EST_PCT = POV_LT200_EST/TOT_EST,
                 POV_LT200_MOE_PCT = pctMOE(TOT_EST,TOT_MOE,POV_LT200_MOE,POV_LT200_EST_PCT),
                 POV_GE200_EST_PCT = POV_GE200_EST/TOT_EST,	
                 POV_GE200_MOE_PCT = pctMOE(TOT_EST,TOT_MOE,POV_GE200_MOE,POV_GE200_EST_PCT)
          )  %>% select(GEOID, NAME, TOT_EST :POV_GE200_MOE, POV_LT100_EST_PCT : POV_GE200_MOE_PCT)
   

   f.pov_est <- f.C17002_FIN %>% select(GEOID, NAME, POV_LT100_EST_PCT,  POV_LT125_EST_PCT, POV_LT200_EST_PCT, POV_GE200_EST_PCT) %>%
       gather(POV_LEVEL, EST_PCT, POV_LT100_EST_PCT :POV_GE200_EST_PCT, factor_key=TRUE) %>%
       mutate(POV_LEVEL = str_replace(POV_LEVEL,"_EST_PCT",""))
   
   f.pov_moe <- f.C17002_FIN %>% select(GEOID,  POV_LT100_MOE_PCT,  POV_LT125_MOE_PCT, POV_LT200_MOE_PCT, POV_GE200_MOE_PCT) %>%
     gather(POV_LEVEL, MOE_PCT, POV_LT100_MOE_PCT :POV_GE200_MOE_PCT, factor_key=TRUE)    %>%
     mutate(POV_LEVEL = str_replace(POV_LEVEL,"_MOE_PCT",""))
   
   f.pov_chart <- inner_join(f.pov_est, f.pov_moe, by=c('GEOID',"POV_LEVEL")) %>%
               mutate(EST_PCT = EST_PCT * 100,
                      MOE_PCT = MOE_PCT * 100)
   f.pov_chart$POV_LEVEL <- factor(f.pov_chart$POV_LEVEL, 
                                    levels = c("POV_LT100",  "POV_LT125", "POV_LT200", "POV_GE200"),
                                    labels = c("Income Less Than 100%",
                                               "Income 100% to 125%",
                                               "Income Less Than 200%",
                                               "Income Greater than 200%"))
   
   nameList <- unique(f.pov_chart$NAME)
   f.pov_chart$NAME <- factor(f.pov_chart$NAME, levels=nameList)
   
   


 chart_list <- list()
 pltTitle <- paste0("Income by Federal Poverty Levels: ", name1)

 
 if(length(cty_str) > 1) {
   f.pov_chart3 <- f.pov_chart %>% filter(!(GEOID %in% cty_str))
 } else {
   f.pov_chart3 <- f.pov_chart
 }
 
 maxLim <- max(f.pov_chart3$EST_PCT) + 10
 
 # Creating Bar Charts
 ggimg3 <-ggplot( f.pov_chart3, aes(x=as.factor(POV_LEVEL), y=EST_PCT, fill= NAME)) +
   geom_bar(stat="identity", position = "dodge", color="black") + 
   geom_text(aes(x=as.factor(POV_LEVEL), y=EST_PCT, label=percent(EST_PCT), group = NAME), 
               position = position_dodge(width=1.0),
               vjust=-0.8,  size=4) +
   scale_fill_manual(values=barCol) +
   scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
   labs(title = pltTitle,
        subtitle = "CSBG Comparison",
        caption = outCap,
        x = "Income as Percentage of Federal Poverty Level",
        y= "Percentage",
        fill="CSBG Entity",
        alt = pltTitle) +
   theme(plot.title = element_text(hjust = 0.5, size=12),
         plot.caption = element_text(hjust = 0, size=9),
         panel.background = element_rect(fill = "white", colour = "gray50"),
         panel.grid.major = element_blank(),
         axis.text.x = element_text(size=10),
         axis.text.y=element_text(size=10),
         legend.position = "bottom", 
         legend.title = element_text(size=8), #change legend title font size
         legend.text = element_text(size=8)) #change legend text font size
 
 chart_list[[1]] <- ggimg3
 
 if(length(cty_str) > 1) {
  for(i in 1:length(cty_str)) {
   n <- i + 1
   f.chart_data <- f.pov_chart %>% filter(GEOID == "080.5" | GEOID == cty_str[i]) 
   unemp_sub <- "County Comparison"

   ggimg3 <-ggplot( f.chart_data, aes(x=as.factor(POV_LEVEL), y=EST_PCT, fill= NAME)) +
     geom_bar(stat="identity", position = "dodge") + 
     geom_text(aes(x=as.factor(POV_LEVEL), y=EST_PCT, label=percent(EST_PCT), group = NAME), 
               position = position_dodge(width=1.0),
               vjust=-0.8,  size=4) +
     scale_fill_manual(values=barCol2) +
     scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
     labs(title = pltTitle,
          subtitle = "County Comparison",
          caption = outCap,
          x = "Income as Percentage of Federal Poverty Level",
          y= "Percentage",
          fill="CSBG Entity",
          alt = pltTitle) +
     theme(plot.title = element_text(hjust = 0.5, size=12),
           plot.caption = element_text(hjust = 0, size=9),
           panel.background = element_rect(fill = "white", colour = "gray50"),
           panel.grid.major = element_blank(),
           axis.text.x = element_text(size=10),
           axis.text.y=element_text(size=10),
           legend.position = "bottom", 
           legend.title = element_text(size=8), #change legend title font size
           legend.text = element_text(size=8)) #change legend text font siz
   
   chart_list[[n]] <- ggimg3
  }
 }
 

 #Creating Table data file

 f.pov_tab_fin <- f.C17002_FIN %>% 
      mutate( 
              POV_LT100_EST = comma_conv(POV_LT100_EST),
              POV_LT100_MOE = comma_conv(POV_LT100_MOE),
              POV_LT100_EST_PCT = percent(POV_LT100_EST_PCT*100),
              POV_LT100_MOE_PCT = percent(POV_LT100_MOE_PCT*100),
              POV_LT125_EST = comma_conv(POV_LT125_EST),
              POV_LT125_MOE = comma_conv(POV_LT125_MOE),
              POV_LT125_EST_PCT = percent(POV_LT125_EST_PCT*100),
              POV_LT125_MOE_PCT = percent(POV_LT125_MOE_PCT*100),
              POV_LT200_EST = comma_conv(POV_LT200_EST),
              POV_LT200_MOE = comma_conv(POV_LT200_MOE),
              POV_LT200_EST_PCT = percent(POV_LT200_EST_PCT*100),
              POV_LT200_MOE_PCT = percent(POV_LT200_MOE_PCT*100),
              POV_GE200_EST = comma_conv(POV_GE200_EST),
              POV_GE200_MOE = comma_conv(POV_GE200_MOE),
              POV_GE200_EST_PCT = percent(POV_GE200_EST_PCT*100),
              POV_GE200_MOE_PCT = percent(POV_GE200_MOE_PCT*100),
              TOT_EST = comma_conv(TOT_EST),
              TOT_MOE = comma_conv(TOT_MOE),
              TOT_EST_PCT = percent(100),
              TOT_MOE_PCT = percent(0)
              ) %>% 
      select("NAME",	"POV_LT100_EST",	"POV_LT100_MOE",	"POV_LT100_EST_PCT",	"POV_LT100_MOE_PCT",
             "POV_LT125_EST",	"POV_LT125_MOE",	"POV_LT125_EST_PCT",	"POV_LT125_MOE_PCT",
             "POV_LT200_EST",	"POV_LT200_MOE",	"POV_LT200_EST_PCT",	"POV_LT200_MOE_PCT",
             "POV_GE200_EST",	"POV_GE200_MOE",	"POV_GE200_EST_PCT",	"POV_GE200_MOE_PCT",
             "TOT_EST",	"TOT_MOE","TOT_EST_PCT",	"TOT_MOE_PCT"	
      )
 

# Flex Table
  tab_head <- paste0("Income by Federal Poverty Levels, ",listID$plName1)
  head_level <- c("","Income Less Than 100%","","","",
                  "Income 100% to 125%","","","",
                  "Income Less Than 200%","","","",
                  "Income Greater than 200%","","","",
                  "Total","","","")
  head_measure <- c("CSBG Entity",rep(c("Estimate","Margin of Error"),10))
  
  headers <-t(bind_cols(as.data.frame(head_level), as.data.frame(head_measure)))
  
    f.povFlex <- flextable(
      f.pov_tab_fin,
       col_keys = names(f.pov_tab_fin)) %>%
       add_header_row(values=head_measure,top=TRUE) %>%
       add_header_row(values=head_level,top=TRUE) %>%
       align(j=1:20,align="center",part="header") %>%
       delete_rows(i = 3, part = "header") %>%
       merge_at(i=1,j=2:5, part="header") %>%
       merge_at(i=1,j=6:9, part="header") %>%
       merge_at(i=1,j=10:13, part="header") %>%
       merge_at(i=1,j=14:17, part="header") %>%
       merge_at(i=1,j=18:21, part="header") %>%
       align(j=1, align="left", part="body") %>%
       align(j=2:21, align="right", part="body") %>%
       width(j=1, width=4) %>%
       width(j=2:21,width=1) 
    
    f.povFlex <- add_header_lines(f.povFlex,values=tab_head, top=TRUE)



  #bind list
  outList <- list("plot"= chart_list, "data" =  f.pov_tab_fin, "header_sh"= headers, "FlexTable" = f.povFlex)
  
  return(outList)
}