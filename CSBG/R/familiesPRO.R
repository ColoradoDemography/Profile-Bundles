#'familiesPRO Outputs Tables and plots for families by FPL and headship
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
familiesPRO <- function(listID,curYr, inData, barCol, barCol2) {
  # Collecting place ids from  idList, setting default values
  # Collecting List of Counties
  outCap <- captionSrc("ACS",curYr,"B17010")
  cty_str <- listID$list2
  full_str <- c("01000","08000",unlist(cty_str))
  cty_str2 <- c("08000",unlist(cty_str))
  cty_name <- CountyName(cty_str)
  cty_num <- as.numeric(str_sub(cty_str,3,5))
  name1 <- listID$plName1
  
  
  # This is a family table, no adjustment for population estimates
 
  
  # POVerty US
  f.B17010_ACS <- inData[["B17010"]] %>% filter(GEOID %in% full_str) %>%
    mutate(
      TOT_TOT_TOT_EST = .[[3]],	
      TOT_TOT_TOT_MOE = .[[4]]^2,
      POV_TOT_TOT_EST = .[[5]],	
      POV_TOT_TOT_MOE = .[[6]]^2,
      POV_MARRIED_TOT_EST = .[[7]],	
      POV_MARRIED_TOT_MOE = .[[8]]^2,
      POV_MARRIED_KIDS_EST = .[[9]],	
      POV_MARRIED_KIDS_MOE = .[[10]]^2,
      POV_MARRIED_NOKIDS_EST = .[[17]],	
      POV_MARRIED_NOKIDS_MOE = .[[18]]^2,
      POV_MALE_TOT_EST = .[[21]],	
      POV_MALE_TOT_MOE = .[[22]]^2,
      POV_MALE_KIDS_EST = .[[23]],	
      POV_MALE_KIDS_MOE = .[[24]]^2,
      POV_MALE_NOKIDS_EST = .[[31]],	
      POV_MALE_NOKIDS_MOE = .[[32]]^2,
      POV_FEMALE_TOT_EST = .[[33]],	
      POV_FEMALE_TOT_MOE = .[[34]]^2,
      POV_FEMALE_KIDS_EST = .[[35]],	
      POV_FEMALE_KIDS_MOE = .[[36]]^2,
      POV_FEMALE_NOKIDS_EST = .[[43]],	
      POV_FEMALE_NOKIDS_MOE = .[[44]]^2,
      POV_TOT_KIDS_EST = .[[9]] + .[[23]] + .[[35]],	
      POV_TOT_KIDS_MOE = .[[10]]^2 + .[[24]]^2 + .[[36]]^2,
      POV_TOT_NOKIDS_EST = .[[17]] + .[[31]] + .[[43]],	
      POV_TOT_NOKIDS_MOE = .[[18]]^2 + .[[32]]^2 + .[[44]]^2,
      
      TOT_MARRIED_TOT_EST = .[[7]] + .[[47]],	
      TOT_MARRIED_TOT_MOE = .[[8]]^2 + .[[48]]^2,
      TOT_MARRIED_KIDS_EST = .[[9]] + .[[49]],	
      TOT_MARRIED_KIDS_MOE = .[[10]]^2 + .[[50]]^2,
      TOT_MARRIED_NOKIDS_EST = .[[17]] + .[[57]],	
      TOT_MARRIED_NOKIDS_MOE = .[[18]]^2 + .[[58]]^2,
      TOT_MALE_TOT_EST = .[[21]] + .[[61]],	
      TOT_MALE_TOT_MOE = .[[22]]^2 + .[[62]]^2,
      TOT_MALE_KIDS_EST = .[[23]] + .[[63]],	
      TOT_MALE_KIDS_MOE = .[[24]]^2 + .[[64]]^2,
      TOT_MALE_NOKIDS_EST = .[[31]] + .[[71]],	
      TOT_MALE_NOKIDS_MOE = .[[32]]^2 + .[[72]]^2,
      TOT_FEMALE_TOT_EST = .[[33]] + .[[73]],	
      TOT_FEMALE_TOT_MOE = .[[34]]^2 + .[[74]]^2,
      TOT_FEMALE_KIDS_EST = .[[35]] + .[[75]],	
      TOT_FEMALE_KIDS_MOE = .[[36]]^2 + .[[76]]^2,
      TOT_FEMALE_NOKIDS_EST = .[[43]] + .[[83]],	
      TOT_FEMALE_NOKIDS_MOE = .[[44]]^2 + .[[84]]^2,
      TOT_TOT_KIDS_EST = .[[9]] + .[[23]] + .[[35]] + .[[49]] + .[[63]] + .[[75]],	
      TOT_TOT_KIDS_MOE = .[[10]]^2 + .[[24]]^2 + .[[36]]^2 + .[[50]]^2 + .[[64]]^2 + .[[76]]^2,
      TOT_TOT_NOKIDS_EST = .[[17]] + .[[31]] + .[[43]] + .[[57]] + .[[71]] + .[[83]],	
      TOT_TOT_NOKIDS_MOE = .[[18]]^2 + .[[32]]^2 + .[[44]]^2 + .[[58]]^2 + .[[72]]^2 +  .[[84]]^2
    )  %>%
    select(GEOID, NAME, TOT_TOT_TOT_EST : TOT_TOT_NOKIDS_MOE)

  
  if(length(cty_str) > 1) {  #Summarize for Multi County Place
    f.B17010_csbg <- f.B17010_ACS %>% filter(GEOID %in% cty_str) %>%
      summarise(TOT_TOT_TOT_EST = sum(TOT_TOT_TOT_EST, na.rm=TRUE),
                TOT_TOT_TOT_MOE = sum(TOT_TOT_TOT_MOE, na.rm=TRUE),
                POV_TOT_TOT_EST = sum(POV_TOT_TOT_EST, na.rm=TRUE),
                POV_TOT_TOT_MOE = sum(POV_TOT_TOT_MOE, na.rm=TRUE),
                POV_MARRIED_TOT_EST = sum(POV_MARRIED_TOT_EST, na.rm=TRUE),
                POV_MARRIED_TOT_MOE = sum(POV_MARRIED_TOT_MOE, na.rm=TRUE),
                POV_MARRIED_KIDS_EST = sum(POV_MARRIED_KIDS_EST, na.rm=TRUE),
                POV_MARRIED_KIDS_MOE = sum(POV_MARRIED_KIDS_MOE, na.rm=TRUE),
                POV_MARRIED_NOKIDS_EST = sum(POV_MARRIED_NOKIDS_EST, na.rm=TRUE),
                POV_MARRIED_NOKIDS_MOE = sum(POV_MARRIED_NOKIDS_MOE, na.rm=TRUE),
                POV_MALE_TOT_EST = sum(POV_MALE_TOT_EST, na.rm=TRUE),
                POV_MALE_TOT_MOE = sum(POV_MALE_TOT_MOE, na.rm=TRUE),
                POV_MALE_KIDS_EST = sum(POV_MALE_KIDS_EST, na.rm=TRUE),
                POV_MALE_KIDS_MOE = sum(POV_MALE_KIDS_MOE, na.rm=TRUE),
                POV_MALE_NOKIDS_EST = sum(POV_MALE_NOKIDS_EST, na.rm=TRUE),
                POV_MALE_NOKIDS_MOE = sum(POV_MALE_NOKIDS_MOE, na.rm=TRUE),
                POV_FEMALE_TOT_EST = sum(POV_FEMALE_TOT_EST, na.rm=TRUE),
                POV_FEMALE_TOT_MOE = sum(POV_FEMALE_TOT_MOE, na.rm=TRUE),
                POV_FEMALE_KIDS_EST = sum(POV_FEMALE_KIDS_EST, na.rm=TRUE),
                POV_FEMALE_KIDS_MOE = sum(POV_FEMALE_KIDS_MOE, na.rm=TRUE),
                POV_FEMALE_NOKIDS_EST = sum(POV_FEMALE_NOKIDS_EST, na.rm=TRUE),
                POV_FEMALE_NOKIDS_MOE = sum(POV_FEMALE_NOKIDS_MOE, na.rm=TRUE),
                POV_TOT_KIDS_EST = sum(POV_TOT_KIDS_EST, na.rm=TRUE),
                POV_TOT_KIDS_MOE = sum(POV_TOT_KIDS_MOE, na.rm=TRUE),  
                POV_TOT_NOKIDS_EST = sum(POV_TOT_NOKIDS_EST, na.rm=TRUE),
                POV_TOT_NOKIDS_MOE = sum(POV_TOT_NOKIDS_MOE, na.rm=TRUE), 
                
                TOT_MARRIED_TOT_EST = sum(TOT_MARRIED_TOT_EST, na.rm=TRUE),
                TOT_MARRIED_TOT_MOE = sum(TOT_MARRIED_TOT_MOE, na.rm=TRUE),
                TOT_MARRIED_KIDS_EST = sum(TOT_MARRIED_KIDS_EST, na.rm=TRUE),
                TOT_MARRIED_KIDS_MOE = sum(TOT_MARRIED_KIDS_MOE, na.rm=TRUE),
                TOT_MARRIED_NOKIDS_EST = sum(TOT_MARRIED_NOKIDS_EST, na.rm=TRUE),
                TOT_MARRIED_NOKIDS_MOE = sum(TOT_MARRIED_NOKIDS_MOE, na.rm=TRUE),
                TOT_MALE_TOT_EST = sum(TOT_MALE_TOT_EST, na.rm=TRUE),
                TOT_MALE_TOT_MOE = sum(TOT_MALE_TOT_MOE, na.rm=TRUE),
                TOT_MALE_KIDS_EST = sum(TOT_MALE_KIDS_EST, na.rm=TRUE),
                TOT_MALE_KIDS_MOE = sum(TOT_MALE_KIDS_MOE, na.rm=TRUE),
                TOT_MALE_NOKIDS_EST = sum(TOT_MALE_NOKIDS_EST, na.rm=TRUE),
                TOT_MALE_NOKIDS_MOE = sum(TOT_MALE_NOKIDS_MOE, na.rm=TRUE),
                TOT_FEMALE_TOT_EST = sum(TOT_FEMALE_TOT_EST, na.rm=TRUE),
                TOT_FEMALE_TOT_MOE = sum(TOT_FEMALE_TOT_MOE, na.rm=TRUE),
                TOT_FEMALE_KIDS_EST = sum(TOT_FEMALE_KIDS_EST, na.rm=TRUE),
                TOT_FEMALE_KIDS_MOE = sum(TOT_FEMALE_KIDS_MOE, na.rm=TRUE),
                TOT_FEMALE_NOKIDS_EST = sum(TOT_FEMALE_NOKIDS_EST, na.rm=TRUE),
                TOT_FEMALE_NOKIDS_MOE = sum(TOT_FEMALE_NOKIDS_MOE, na.rm=TRUE),
                TOT_TOT_KIDS_EST = sum(TOT_TOT_KIDS_EST, na.rm=TRUE),
                TOT_TOT_KIDS_MOE = sum(TOT_TOT_KIDS_MOE, na.rm=TRUE),
                TOT_TOT_NOKIDS_EST = sum(TOT_TOT_NOKIDS_EST, na.rm=TRUE),
                TOT_TOT_NOKIDS_MOE = sum(TOT_TOT_NOKIDS_MOE, na.rm=TRUE)                
                ) %>%
      mutate(GEOID = "08000.5",
             NAME = name1) %>%
      select(GEOID, NAME, TOT_TOT_TOT_EST : TOT_TOT_NOKIDS_MOE)
    
    f.B17010_US <- f.B17010_ACS %>% filter(GEOID == "01000")
    f.B17010_CO <- f.B17010_ACS %>% filter(GEOID == "08000")
    f.B17010_CTY <- f.B17010_ACS %>% filter(GEOID %in% cty_str)
    
    f.B17010_FIN <- bind_rows(f.B17010_US, f.B17010_CO, f.B17010_csbg, f.B17010_CTY)
  } else {
    f.B17010_FIN <- f.B17010_ACS %>% filter(GEOID %in% full_str)
  }

  # calculating Percentage
  f.B17010_FIN <- f.B17010_FIN %>%
    mutate(NAME = str_replace(NAME, ", Colorado",""),
           TOT_TOT_TOT_MOE = sqrt(TOT_TOT_TOT_MOE),
           POV_TOT_TOT_MOE = sqrt(POV_TOT_TOT_MOE),
           POV_MARRIED_TOT_MOE = sqrt(POV_MARRIED_TOT_MOE),
           POV_MARRIED_KIDS_MOE = sqrt(POV_MARRIED_KIDS_MOE),
           POV_MARRIED_NOKIDS_MOE = sqrt(POV_MARRIED_NOKIDS_MOE),
           POV_MALE_TOT_MOE = sqrt(POV_MALE_TOT_MOE),
           POV_MALE_KIDS_MOE = sqrt(POV_MALE_KIDS_MOE),
           POV_MALE_NOKIDS_MOE = sqrt(POV_MALE_NOKIDS_MOE),
           POV_FEMALE_TOT_MOE = sqrt(POV_FEMALE_TOT_MOE),
           POV_FEMALE_KIDS_MOE = sqrt(POV_FEMALE_KIDS_MOE),
           POV_FEMALE_NOKIDS_MOE = sqrt(POV_FEMALE_NOKIDS_MOE),
           POV_TOT_KIDS_MOE = sqrt(POV_TOT_KIDS_MOE),
           POV_TOT_NOKIDS_MOE = sqrt(POV_TOT_NOKIDS_MOE),
           
           TOT_MARRIED_TOT_MOE = sqrt(TOT_MARRIED_TOT_MOE),
           TOT_MARRIED_KIDS_MOE = sqrt(TOT_MARRIED_KIDS_MOE),
           TOT_MARRIED_NOKIDS_MOE = sqrt(TOT_MARRIED_NOKIDS_MOE),
           TOT_MALE_TOT_MOE = sqrt(TOT_MALE_TOT_MOE),
           TOT_MALE_KIDS_MOE = sqrt(TOT_MALE_KIDS_MOE),
           TOT_MALE_NOKIDS_MOE = sqrt(TOT_MALE_NOKIDS_MOE),
           TOT_FEMALE_TOT_MOE = sqrt(TOT_FEMALE_TOT_MOE),
           TOT_FEMALE_KIDS_MOE = sqrt(TOT_FEMALE_KIDS_MOE),
           TOT_FEMALE_NOKIDS_MOE = sqrt(TOT_FEMALE_NOKIDS_MOE),
           TOT_TOT_KIDS_MOE = sqrt(TOT_TOT_KIDS_MOE),
           TOT_TOT_NOKIDS_MOE = sqrt(TOT_TOT_NOKIDS_MOE),
           
           
           POV_MARRIED_KIDS_EST_PCT = POV_MARRIED_KIDS_EST/POV_MARRIED_TOT_EST,
           POV_MARRIED_KIDS_MOE_PCT =  pctMOE(POV_MARRIED_TOT_EST,POV_MARRIED_TOT_MOE,POV_MARRIED_KIDS_MOE,POV_MARRIED_KIDS_EST_PCT),
           POV_MARRIED_NOKIDS_EST_PCT = POV_MARRIED_NOKIDS_EST/POV_MARRIED_TOT_EST,
           POV_MARRIED_NOKIDS_MOE_PCT =  pctMOE(POV_MARRIED_TOT_EST,POV_MARRIED_TOT_MOE,POV_MARRIED_NOKIDS_MOE,POV_MARRIED_NOKIDS_EST_PCT),
           POV_MALE_KIDS_EST_PCT = POV_MALE_KIDS_EST/POV_MALE_TOT_EST,
           POV_MALE_KIDS_MOE_PCT =  pctMOE(POV_MALE_TOT_EST,POV_MALE_TOT_MOE,POV_MALE_KIDS_MOE,POV_MALE_KIDS_EST_PCT),
           POV_MALE_NOKIDS_EST_PCT = POV_MALE_NOKIDS_EST/POV_MALE_TOT_EST,
           POV_MALE_NOKIDS_MOE_PCT =  pctMOE(POV_MALE_TOT_EST,POV_MALE_TOT_MOE,POV_MALE_NOKIDS_MOE,POV_MALE_NOKIDS_EST_PCT),
           POV_FEMALE_KIDS_EST_PCT = POV_FEMALE_KIDS_EST/POV_FEMALE_TOT_EST,
           POV_FEMALE_KIDS_MOE_PCT =  pctMOE(POV_FEMALE_TOT_EST,POV_FEMALE_TOT_MOE,POV_FEMALE_KIDS_MOE,POV_FEMALE_KIDS_EST_PCT),
           POV_FEMALE_NOKIDS_EST_PCT = POV_FEMALE_NOKIDS_EST/POV_FEMALE_TOT_EST,
           POV_FEMALE_NOKIDS_MOE_PCT =  pctMOE(POV_FEMALE_TOT_EST,POV_FEMALE_TOT_MOE,POV_FEMALE_NOKIDS_MOE,POV_FEMALE_NOKIDS_EST_PCT),
           POV_TOT_KIDS_EST_PCT = POV_TOT_KIDS_EST/POV_TOT_TOT_EST,
           POV_TOT_KIDS_MOE_PCT =  pctMOE(POV_TOT_TOT_EST,POV_TOT_TOT_MOE,POV_TOT_KIDS_MOE,POV_TOT_KIDS_EST_PCT),
           POV_TOT_NOKIDS_EST_PCT = POV_TOT_NOKIDS_EST/POV_TOT_TOT_EST,
           POV_TOT_NOKIDS_MOE_PCT =  pctMOE(POV_TOT_TOT_EST,POV_TOT_TOT_MOE,POV_TOT_NOKIDS_MOE,POV_TOT_NOKIDS_EST_PCT),
           
 
           TOT_MARRIED_KIDS_EST_PCT = TOT_MARRIED_KIDS_EST/TOT_MARRIED_TOT_EST,
           TOT_MARRIED_KIDS_MOE_PCT =  pctMOE(TOT_MARRIED_TOT_EST,TOT_MARRIED_TOT_MOE,TOT_MARRIED_KIDS_MOE,TOT_MARRIED_KIDS_EST_PCT),
           TOT_MARRIED_NOKIDS_EST_PCT = TOT_MARRIED_NOKIDS_EST/TOT_MARRIED_TOT_EST,
           TOT_MARRIED_NOKIDS_MOE_PCT =  pctMOE(TOT_MARRIED_TOT_EST,TOT_MARRIED_TOT_MOE,TOT_MARRIED_NOKIDS_MOE,TOT_MARRIED_NOKIDS_EST_PCT),
           TOT_MALE_KIDS_EST_PCT = TOT_MALE_KIDS_EST/TOT_MALE_TOT_EST,
           TOT_MALE_KIDS_MOE_PCT =  pctMOE(TOT_MALE_TOT_EST,TOT_MALE_TOT_MOE,TOT_MALE_KIDS_MOE,TOT_MALE_KIDS_EST_PCT),
           TOT_MALE_NOKIDS_EST_PCT = TOT_MALE_NOKIDS_EST/TOT_MALE_TOT_EST,
           TOT_MALE_NOKIDS_MOE_PCT =  pctMOE(TOT_MALE_TOT_EST,TOT_MALE_TOT_MOE,TOT_MALE_NOKIDS_MOE,TOT_MALE_NOKIDS_EST_PCT),
           TOT_FEMALE_KIDS_EST_PCT = TOT_FEMALE_KIDS_EST/TOT_FEMALE_TOT_EST,
           TOT_FEMALE_KIDS_MOE_PCT =  pctMOE(TOT_FEMALE_TOT_EST,TOT_FEMALE_TOT_MOE,TOT_FEMALE_KIDS_MOE,TOT_FEMALE_KIDS_EST_PCT),
           TOT_FEMALE_NOKIDS_EST_PCT = TOT_FEMALE_NOKIDS_EST/TOT_FEMALE_TOT_EST,
           TOT_FEMALE_NOKIDS_MOE_PCT =  pctMOE(TOT_FEMALE_TOT_EST,TOT_FEMALE_TOT_MOE,TOT_FEMALE_NOKIDS_MOE,TOT_FEMALE_NOKIDS_EST_PCT),
           TOT_TOT_KIDS_EST_PCT = TOT_TOT_KIDS_EST/TOT_TOT_TOT_EST,
           TOT_TOT_KIDS_MOE_PCT =  pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,TOT_TOT_KIDS_MOE,TOT_TOT_KIDS_EST_PCT),
           TOT_TOT_NOKIDS_EST_PCT = TOT_TOT_NOKIDS_EST/TOT_TOT_TOT_EST,
           TOT_TOT_NOKIDS_MOE_PCT =  pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,TOT_TOT_NOKIDS_MOE,TOT_TOT_NOKIDS_EST_PCT)
           )  %>% 
     arrange(GEOID) %>%
     select(GEOID, NAME, TOT_TOT_TOT_EST :TOT_TOT_NOKIDS_MOE_PCT)
  

  # Building Chart data sets
  f.fam_est <- f.B17010_FIN %>% select(GEOID, NAME, 
                                       POV_FEMALE_KIDS_EST_PCT,	POV_FEMALE_NOKIDS_EST_PCT,	POV_MALE_KIDS_EST_PCT,	
                                       POV_MALE_NOKIDS_EST_PCT,	POV_MARRIED_KIDS_EST_PCT,	POV_MARRIED_NOKIDS_EST_PCT,
                                       POV_TOT_KIDS_EST_PCT, POV_TOT_NOKIDS_EST_PCT,
                                       TOT_FEMALE_KIDS_EST_PCT,	TOT_FEMALE_NOKIDS_EST_PCT,	TOT_MALE_KIDS_EST_PCT,	
                                       TOT_MALE_NOKIDS_EST_PCT,	TOT_MARRIED_KIDS_EST_PCT,	TOT_MARRIED_NOKIDS_EST_PCT,
                                       TOT_TOT_KIDS_EST_PCT, TOT_TOT_NOKIDS_EST_PCT) %>%
    gather(FAM_TYPE, EST_PCT,POV_FEMALE_KIDS_EST_PCT : TOT_TOT_NOKIDS_EST_PCT, factor_key =TRUE) %>%
    mutate(FAM_TYPE = str_replace(FAM_TYPE,"_EST_PCT",""))
  
  f.fam_moe <- f.B17010_FIN %>% select(GEOID, NAME, 
                                       POV_FEMALE_KIDS_MOE_PCT,	POV_FEMALE_NOKIDS_MOE_PCT,	POV_MALE_KIDS_MOE_PCT,	
                                       POV_MALE_NOKIDS_MOE_PCT,	POV_MARRIED_KIDS_MOE_PCT,	POV_MARRIED_NOKIDS_MOE_PCT,
                                       POV_TOT_KIDS_MOE_PCT, POV_TOT_NOKIDS_MOE_PCT,
                                       TOT_FEMALE_KIDS_MOE_PCT,	TOT_FEMALE_NOKIDS_MOE_PCT,	TOT_MALE_KIDS_MOE_PCT,	
                                       TOT_MALE_NOKIDS_MOE_PCT,	TOT_MARRIED_KIDS_MOE_PCT,	TOT_MARRIED_NOKIDS_MOE_PCT,
                                       TOT_TOT_KIDS_MOE_PCT, TOT_TOT_NOKIDS_MOE_PCT) %>%
    gather(FAM_TYPE, MOE_PCT,POV_FEMALE_KIDS_MOE_PCT : TOT_TOT_NOKIDS_MOE_PCT, factor_key =TRUE) %>%
    mutate(FAM_TYPE = str_replace(FAM_TYPE,"_MOE_PCT","")) %>% select(-NAME)
  

  f.fam_chart <- inner_join(f.fam_est, f.fam_moe, by =c('GEOID',"FAM_TYPE"))   %>%
    mutate(EST_PCT = EST_PCT * 100,
           MOE_PCT = MOE_PCT * 100,
           TYPE = ifelse(str_sub(FAM_TYPE,1,3) == "POV",0,1),
           CHILDREN = ifelse(grepl("NOKIDS",FAM_TYPE),1,0),
           FAM_TYPE = str_sub(FAM_TYPE,5,nchar(FAM_TYPE)),
           FAM_TYPE = str_replace(FAM_TYPE,"_NOKIDS",""),
           FAM_TYPE = str_replace(FAM_TYPE,"_KIDS","")) %>% replace(is.na(.),0)
  f.fam_chart_tab <- f.fam_chart
  
  
  f.fam_chart$FAM_TYPE <- factor(f.fam_chart$FAM_TYPE, 
                                  levels = c("FEMALE",  "MALE", "MARRIED", "TOT"),
                                  labels = c("Female Householder\nNo Spouse Present",
                                             "Male Householder\nNo Spouse Present",
                                             "Married Couple Families",
                                             "All Families"))
  
  f.fam_chart$TYPE <- factor(f.fam_chart$TYPE, levels=c(0,1),
                              labels =c("Below Federal Poverty Level",
                                        "Total Population"))
  f.fam_chart$CHILDREN <- factor(f.fam_chart$CHILDREN, levels=c(0,1),
                             labels =c("Children Present",
                                       "No Children Present"))
  
  nameList <- unique(f.fam_chart$NAME)
  f.fam_chart$NAME <- factor(f.fam_chart$NAME, levels =nameList)
  
  chart_list <- list()
  pltTitle <- paste0("Family Type by Poverty Status: ", name1)

 if(length(cty_str) > 1) {
   f.fam_chart3 <- f.fam_chart %>% filter(!(GEOID %in% cty_str))
 } else {
   f.fam_chart3 <- f.fam_chart
 }

  barCol6 <- c("#359A7E", "#7FFF00", "#6D3A5D", "#F08080", "#007ADE", "#ADD8E6")
  barCol4 <- c("#007ADE", "#ADD8E6", "#C0504D", "#F08080")
 
  # Creating Bar Charts
  ggimg3 <-ggplot( f.fam_chart3, aes(x =as.factor(FAM_TYPE), y =EST_PCT, fill = interaction(CHILDREN, NAME))) +
    geom_bar(stat ="identity", position = "dodge", color ="black") + 
    geom_text(aes(x =as.factor(FAM_TYPE), y =EST_PCT, label =percent(EST_PCT), group = interaction(CHILDREN, NAME)), 
              position = position_dodge(width =1.0),
              vjust =-0.8, size = 2) +
    scale_fill_manual(values =barCol6) +
    scale_y_continuous(breaks = seq(0, 100, by=20), limits = c(0, 120), label =percent,  expand = c(0, 0)) +
  facet_wrap(vars(TYPE), ncol=1) +
    labs(title = pltTitle,
         subtitle = "CSBG Comparison",
         caption = outCap,
         x = "Family Type",
         y = "Percentage",
         fill ="Presence of Children\nCSBG Entity",
         alt = pltTitle) +
    guides(fill=guide_legend(nrow=3, ncol=2, byrow=TRUE))  +
    theme(plot.title = element_text(hjust = 0.5, size =12),
          plot.caption = element_text(hjust = 0, size =9),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_blank(),
          axis.text.x = element_text(size =10),
          axis.text.y =element_text(size =10),
          legend.position = "bottom", 
          legend.title = element_text(size=8), #change legend title font size
          legend.text = element_text(size=8)) #change legend text font size
  
  chart_list[[1]] <- ggimg3
  
  if(length(cty_str) >1 ){
  for(i in 1:length(cty_str)) {
    n <- i + 1
    f.chart_data <- f.fam_chart %>% filter(GEOID == "08000.5" | GEOID == cty_str[i]) 
    
    ggimg3 <-ggplot(  f.chart_data, aes(x =as.factor(FAM_TYPE), y =EST_PCT, fill = interaction(CHILDREN, NAME))) +
      geom_bar(stat ="identity", position = "dodge", color ="black") + 
      geom_text(aes(x =as.factor(FAM_TYPE), y =EST_PCT, label =percent(EST_PCT), group = interaction(CHILDREN, NAME)), 
                position = position_dodge(width =1.0),
                vjust =-0.8,  size = 2) +
      scale_fill_manual(values =barCol4) +
      scale_y_continuous(breaks = seq(0, 100, by=20), limits = c(0, 120), label =percent,  expand = c(0, 0)) +
      facet_wrap(vars(TYPE), ncol=1) +
      labs(title = pltTitle,
           subtitle = "County Comparison",
           caption = outCap,
           x = "Family Type",
           y = "Percentage",
           fill ="Presence of Children\nCSBG Entity",
           alt = pltTitle) +
      guides(fill=guide_legend(nrow=2, ncol=2, byrow=TRUE))  +
      theme(plot.title = element_text(hjust = 0.5, size =12),
            plot.caption = element_text(hjust = 0, size =9),
            panel.background = element_rect(fill = "white", colour = "gray50"),
            panel.grid.major = element_blank(),
            axis.text.x = element_text(size =10),
            axis.text.y =element_text(size =10),
            legend.position = "bottom", 
            legend.title = element_text(size=8), #change legend title font size
            legend.text = element_text(size=8)) #change legend text font siz
    
    chart_list[[n]] <- ggimg3
  }
  }  

  #Creating Table data file
  # selecting Total Povery Percentages
  

  
if(length(cty_str) > 1) {
  name_list <- unique(c("United States","Colorado",name1,cty_name))
} else {
  name_list <- c("United States","Colorado",cty_name)
 }
 
  f.B17010_FIN$NAME <- factor(f.B17010_FIN$NAME ,levels=name_list)
  
  f.fam_pov <- f.B17010_FIN %>% 
    mutate(TYPE = 0) %>%
    select(GEOID, NAME, TYPE, 
           POV_FEMALE_KIDS_EST,	POV_FEMALE_KIDS_MOE,	POV_FEMALE_KIDS_EST_PCT, POV_FEMALE_KIDS_MOE_PCT,
           POV_FEMALE_NOKIDS_EST,	POV_FEMALE_NOKIDS_MOE,	POV_FEMALE_NOKIDS_EST_PCT, POV_FEMALE_NOKIDS_MOE_PCT,	
           POV_FEMALE_TOT_EST,	POV_FEMALE_TOT_MOE,	
           POV_MALE_KIDS_EST,	POV_MALE_KIDS_MOE, POV_MALE_KIDS_EST_PCT,	POV_MALE_KIDS_MOE_PCT,	
           POV_MALE_NOKIDS_EST,	POV_MALE_NOKIDS_MOE, POV_MALE_NOKIDS_EST_PCT,	POV_MALE_NOKIDS_MOE_PCT,	
           POV_MALE_TOT_EST,	POV_MALE_TOT_MOE,	
           POV_MARRIED_KIDS_EST,	POV_MARRIED_KIDS_MOE,	POV_MARRIED_KIDS_EST_PCT,	POV_MARRIED_KIDS_MOE_PCT,	
           POV_MARRIED_NOKIDS_EST,	POV_MARRIED_NOKIDS_MOE,	POV_MARRIED_NOKIDS_EST_PCT,	POV_MARRIED_NOKIDS_MOE_PCT,	
           POV_MARRIED_TOT_EST,	POV_MARRIED_TOT_MOE,	
           POV_TOT_KIDS_EST,	POV_TOT_KIDS_MOE,	POV_TOT_KIDS_EST_PCT,	POV_TOT_KIDS_MOE_PCT,	
           POV_TOT_NOKIDS_EST,	POV_TOT_NOKIDS_MOE,	POV_TOT_NOKIDS_EST_PCT,	POV_TOT_NOKIDS_MOE_PCT,	
           POV_TOT_TOT_EST,	POV_TOT_TOT_MOE)
  names(f.fam_pov) <- c("GEOID", "NAME",	"TYPE",	
                        "FEMALE_KIDS_EST",	"FEMALE_KIDS_MOE",	"FEMALE_KIDS_EST_PCT", "FEMALE_KIDS_MOE_PCT",	
                        "FEMALE_NOKIDS_EST",	"FEMALE_NOKIDS_MOE",	"FEMALE_NOKIDS_EST_PCT", "FEMALE_NOKIDS_MOE_PCT",	
                        "FEMALE_TOT_EST",	"FEMALE_TOT_MOE",	
                        "MALE_KIDS_EST",	"MALE_KIDS_MOE", "MALE_KIDS_EST_PCT",	"MALE_KIDS_MOE_PCT",	
                        "MALE_NOKIDS_EST",	"MALE_NOKIDS_MOE", "MALE_NOKIDS_EST_PCT",	"MALE_NOKIDS_MOE_PCT",	
                        "MALE_TOT_EST",	"MALE_TOT_MOE",	
                        "MARRIED_KIDS_EST",	"MARRIED_KIDS_MOE",	"MARRIED_KIDS_EST_PCT",	"MARRIED_KIDS_MOE_PCT",	
                        "MARRIED_NOKIDS_EST",	"MARRIED_NOKIDS_MOE",	"MARRIED_NOKIDS_EST_PCT",	"MARRIED_NOKIDS_MOE_PCT",	
                        "MARRIED_TOT_EST",	"MARRIED_TOT_MOE",	
                        "TOT_KIDS_EST",	"TOT_KIDS_MOE",	"TOT_KIDS_EST_PCT",	"TOT_KIDS_MOE_PCT",	
                        "TOT_NOKIDS_EST",	"TOT_NOKIDS_MOE",	"TOT_NOKIDS_EST_PCT",	"TOT_NOKIDS_MOE_PCT",	
                        "TOT_TOT_EST",	"TOT_TOT_MOE")

  
  f.fam_tot <- f.B17010_FIN %>% 
    mutate(TYPE = 1) %>%
    select(GEOID, NAME, TYPE, 
           TOT_FEMALE_KIDS_EST,	TOT_FEMALE_KIDS_MOE,	TOT_FEMALE_KIDS_EST_PCT, TOT_FEMALE_KIDS_MOE_PCT,
           TOT_FEMALE_NOKIDS_EST,	TOT_FEMALE_NOKIDS_MOE,	TOT_FEMALE_NOKIDS_EST_PCT, TOT_FEMALE_NOKIDS_MOE_PCT,	
           TOT_FEMALE_TOT_EST,	TOT_FEMALE_TOT_MOE,	
           TOT_MALE_KIDS_EST,	TOT_MALE_KIDS_MOE, TOT_MALE_KIDS_EST_PCT,	TOT_MALE_KIDS_MOE_PCT,	
           TOT_MALE_NOKIDS_EST,	TOT_MALE_NOKIDS_MOE, TOT_MALE_NOKIDS_EST_PCT,	TOT_MALE_NOKIDS_MOE_PCT,	
           TOT_MALE_TOT_EST,	TOT_MALE_TOT_MOE,	
           TOT_MARRIED_KIDS_EST,	TOT_MARRIED_KIDS_MOE,	TOT_MARRIED_KIDS_EST_PCT,	TOT_MARRIED_KIDS_MOE_PCT,	
           TOT_MARRIED_NOKIDS_EST,	TOT_MARRIED_NOKIDS_MOE,	TOT_MARRIED_NOKIDS_EST_PCT,	TOT_MARRIED_NOKIDS_MOE_PCT,	
           TOT_MARRIED_TOT_EST,	TOT_MARRIED_TOT_MOE,	
           TOT_TOT_KIDS_EST,	TOT_TOT_KIDS_MOE,	TOT_TOT_KIDS_EST_PCT,	TOT_TOT_KIDS_MOE_PCT,	
           TOT_TOT_NOKIDS_EST,	TOT_TOT_NOKIDS_MOE,	TOT_TOT_NOKIDS_EST_PCT,	TOT_TOT_NOKIDS_MOE_PCT,	
           TOT_TOT_TOT_EST,	TOT_TOT_TOT_MOE)
  names(f.fam_tot) <- c("GEOID", "NAME",	"TYPE",	
                        "FEMALE_KIDS_EST",	"FEMALE_KIDS_MOE",	"FEMALE_KIDS_EST_PCT", "FEMALE_KIDS_MOE_PCT",	
                        "FEMALE_NOKIDS_EST",	"FEMALE_NOKIDS_MOE",	"FEMALE_NOKIDS_EST_PCT", "FEMALE_NOKIDS_MOE_PCT",	
                        "FEMALE_TOT_EST",	"FEMALE_TOT_MOE",	
                        "MALE_KIDS_EST",	"MALE_KIDS_MOE", "MALE_KIDS_EST_PCT",	"MALE_KIDS_MOE_PCT",	
                        "MALE_NOKIDS_EST",	"MALE_NOKIDS_MOE", "MALE_NOKIDS_EST_PCT",	"MALE_NOKIDS_MOE_PCT",	
                        "MALE_TOT_EST",	"MALE_TOT_MOE",	
                        "MARRIED_KIDS_EST",	"MARRIED_KIDS_MOE",	"MARRIED_KIDS_EST_PCT",	"MARRIED_KIDS_MOE_PCT",	
                        "MARRIED_NOKIDS_EST",	"MARRIED_NOKIDS_MOE",	"MARRIED_NOKIDS_EST_PCT",	"MARRIED_NOKIDS_MOE_PCT",	
                        "MARRIED_TOT_EST",	"MARRIED_TOT_MOE",	
                        "TOT_KIDS_EST",	"TOT_KIDS_MOE",	"TOT_KIDS_EST_PCT",	"TOT_KIDS_MOE_PCT",	
                        "TOT_NOKIDS_EST",	"TOT_NOKIDS_MOE",	"TOT_NOKIDS_EST_PCT",	"TOT_NOKIDS_MOE_PCT",	
                        "TOT_TOT_EST",	"TOT_TOT_MOE")
  
  

  f.fam_tab_fin_l <- bind_rows(f.fam_pov, f.fam_tot) %>%
    arrange(GEOID, TYPE) %>% select(-GEOID)
    
  f.fam_tab_fin_l[,c(3,4,7,8,11,12,13,14,17,18,21,22,23,24,27,28,31,32,33,34,37,38,41,42)] <-
          sapply(f.fam_tab_fin_l[,c(3,4,7,8,11,12,13,14,17,18,21,22,23,24,27,28,31,32,33,34,37,38,41,42)], 
                 function(x) comma_conv(x))
  f.fam_tab_fin_l[,c(5,6,9,10,15,16,19,20,25,26,29,30,35,36,39,40)] <-
         sapply(f.fam_tab_fin_l[,c(5,6,9,10,15,16,19,20,25,26,29,30,35,36,39,40)], function(x) percent(x * 100))
 
  f.fam_tab_fin_l$NAME <- as.character(f.fam_tab_fin_l$NAME)
  for(i in 1:nrow(f.fam_tab_fin_l)) {
    if(i %% 2 == 0) {
      f.fam_tab_fin_l[i,1] = ""
    }
  }
  
  f.fam_tab_fin_l$TYPE <- factor(f.fam_tab_fin_l$TYPE, levels=c(0,1),
                                  labels =c("Families Below Federal Poverty Level",
                                            "All Families"))
  
  # Flex Table
  tab_head <- paste0("Families by Poverty Status, ",listID$plName1)
  head_level1 <- c("",	"",	"Female Householder, No Spouse Present",	"",	"",	"",	"",	"",	"",	"",	"",	"",	
                   "Male Householder, No Spouse Present",	"",	"",	"",	"",	"",	"",	"",	"",	"",	
                   "Married Couple Families",	"",	"",	"",	"",	"",	"",	"",	"",	"",	
                   "All Families",	"",	"",	"",	"",	"",	"",	"",	"",	"")
  head_level2 <- c("",	"",	"Children Present",	"",	"",	"",	"No Children Present",	"",	"",	"",	"All Families",	"",	
                   "Children Present",	"",	"",	"",	"No Children Present",	"",	"",	"",	"All Families",	"",	
                   "Children Present",	"",	"",	"",	"No Children Present",	"",	"",	"",	"All Families",	"",	
                   "Children Present",	"",	"",	"",	"No Children Present",	"",	"",	"",	"All Families",	"")
  head_measure <- c("CSBG Entity","Poverty Status",rep(c("Estimate","Margin of Error"),20))
  
  headers <-t(bind_cols(as.data.frame(head_level1), as.data.frame(head_level2), as.data.frame(head_measure)))
  
  f.famFlex <- flextable(
    f.fam_tab_fin_l,
    col_keys = names(f.fam_tab_fin_l)) %>%
    add_header_row(values =head_measure,top =TRUE) %>%
    add_header_row(values =head_level2,top =TRUE) %>%
    add_header_row(values =head_level1,top =TRUE) %>%
    align(j =1:22,align ="center",part ="header") %>%
    delete_rows(i = 4, part = "header") %>%
    merge_at(i =1,j =3:12, part ="header") %>%
    merge_at(i =1,j =13:22, part ="header") %>%
    merge_at(i =1,j =23:32, part ="header") %>%
    merge_at(i =1,j =33:42, part ="header") %>%
    merge_at(i =2,j =3:6, part ="header") %>%
    merge_at(i =2,j =7:10, part ="header") %>%
    merge_at(i =2,j =11:12, part ="header") %>%
    merge_at(i =2,j =13:16, part ="header") %>%
    merge_at(i =2,j =17:20, part ="header") %>%
    merge_at(i =2,j =21:22, part ="header") %>%
    merge_at(i =2,j =23:26, part ="header") %>%
    merge_at(i =2,j =27:30, part ="header") %>%
    merge_at(i =2,j =31:32, part ="header") %>%
    merge_at(i =2,j =33:36, part ="header") %>%
    merge_at(i =2,j =37:40, part ="header") %>%
    merge_at(i =2,j =41:42, part ="header") %>%
    align(j =1:2, align ="left", part ="body") %>%
    align(j =3:42, align ="right", part ="body") %>%
    width(j =1:2, width =4) %>%
    width(j =3:42,width =1) 
  
  f.famFlex <- add_header_lines(f.famFlex,values =tab_head, top =TRUE)
  
  
  
  #bind list
  outList <- list("plot" = chart_list, "data" =  f.fam_tab_fin_l, "header_sh" = headers, "FlexTable" = f.famFlex)
  
  return(outList)
}
