#'disabilityT7 Outputs Tables and plots for the Disability by Age and FPL
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
disabilityT7 <- function(DBPool, listID, curYr, censAPI, barCol, barCol2) {
  # Collecting place ids from  idList, setting default values
  # Collecting List of Counties
  outCap <- captionSrc("ACS",curYr,"C18130") 
  cty_str <- listID$list2
  cty_name <- CountyName(cty_str)
  cty_num <- as.numeric(str_sub(cty_str,3,5))
  name1 <- listID$plName1
  
  
  # Collecting US Data 
  f.USage <- read_csv("data/nc-est2023-agesex-res.csv") 
  
  f.USage_grp <- f.USage %>%  filter(AGE != 999) %>% 
    filter(SEX == 0) %>%
    mutate(age_group = ifelse(AGE <= 17,"AGELT18_EST",
                              ifelse(AGE <= 64, "AGE1864_EST","AGEGE65_EST"))
                              ) %>%
    group_by(age_group) %>%
    summarize(totalpopulation = sum(POPESTIMATE2023, na.rm=TRUE)) %>%
    mutate(countyfips = -100,
           countyname = "United States",
           GEOID ='01000') %>%
    spread(age_group, totalpopulation) %>%
    mutate(TOTAL_EST = AGELT18_EST + AGE1864_EST + AGEGE65_EST ) %>%
    select(GEOID, countyfips, countyname,
           AGELT18_EST, AGE1864_EST, AGEGE65_EST, TOTAL_EST) 
  
  
  
  
  ctyList <- listID$list2
  ctynum <- c(0,as.numeric(str_sub(ctyList,3,5)))
  ctylist2 <- paste(as.character(ctynum), sep="' '", collapse=", ")
  
  SQLUrl <- paste0("SELECT countyfips, county, year, age,  totalpopulation FROM estimates.county_sya WHERE countyfips IN (",ctylist2,") AND year = ",curYr,";")
  f.COAge <- dbGetQuery(DBPool, SQLUrl) 
  
  f.COAge_long <- f.COAge %>%
    mutate(age_group = ifelse(age <= 17,"AGELT18_EST",
                              ifelse(age <= 64, "AGE1864_EST", "AGEGE65_EST")))   %>%
    group_by(countyfips, county, age_group) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    mutate( countyname = ifelse(county == "Colorado","Colorado",paste0(county, " County")),
            GEOID = paste0("08",str_pad(countyfips,3,pad="0"))) %>%
    spread(age_group, totalpopulation) %>%
    mutate(TOTAL_EST = AGELT18_EST + AGE1864_EST + AGEGE65_EST ) %>%
    select(GEOID, countyfips, countyname,
           AGELT18_EST, AGE1864_EST, AGEGE65_EST, TOTAL_EST) 
  
  
  if(length(listID$list2) > 1){  #Summing up CSBG Entity
    f.csbg_age <- f.COAge_long %>% filter(countyfips != 0) %>%
      mutate(year = 1) %>%
      group_by(year) %>%
      summarize(AGELT18_EST = sum(AGELT18_EST),
                AGE1864_EST = sum(AGE1864_EST),
                AGEGE65_EST = sum(AGEGE65_EST),                
                TOTAL_EST = sum(TOTAL_EST))  %>%
      mutate(countyfips = 0.500,
             countyname = listID$plName1,
             GEOID = "08000.5") %>%
      select(GEOID, countyfips, countyname,
             AGELT18_EST, AGE1864_EST, AGEGE65_EST, TOTAL_EST) 
    
    f.state_age <- f.COAge_long %>% filter(countyfips == 0)
    f.cty_age  <- f.COAge_long %>% filter(countyfips != 0) 
    f.age_sya <- bind_rows(f.USage_grp, f.state_age, f.csbg_age, f.cty_age) %>% select(-county)
  } else {
    f.age_sya <- bind_rows(f.USage_grp, f.COAge_long) %>% select(-county)
  }
  
  
  
  # POVerty US
  f.C18130_US <- get_acs(geography = "us", 
                         table = 'C18130', 
                         survey = "acs5",
                         year = curYr,
                         key = censAPI,
                         output ="wide") %>%
    mutate(
      GEOID = paste0("0",GEOID,"000"),
      TOT_TOT_TOT_EST  = .[[5]],	
      TOT_TOT_TOT_MOE  = .[[6]]^2,
      TOT_TOT_LE18_EST  = .[[7]],	
      TOT_TOT_LE18_MOE  = .[[8]]^2,
      TOT_DIS_LE18_EST  = .[[9]],	
      TOT_DIS_LE18_MOE  = .[[10]]^2,
      POV_DIS_LE18_EST  = .[[11]],	
      POV_DIS_LE18_MOE  = .[[12]]^2,
      TOT_TOT_1864_EST  = .[[21]],	
      TOT_TOT_1864_MOE  = .[[22]]^2,
      TOT_DIS_1864_EST  = .[[23]],	
      TOT_DIS_1864_MOE  = .[[24]]^2,
      POV_DIS_1864_EST  = .[[25]],	
      POV_DIS_1864_MOE  = .[[26]]^2,
      TOT_TOT_GE65_EST  = .[[35]],	
      TOT_TOT_GE65_MOE  = .[[36]]^2,
      TOT_DIS_GE65_EST  = .[[37]],	
      TOT_DIS_GE65_MOE  = .[[38]]^2,
      POV_DIS_GE65_EST  = .[[39]],	
      POV_DIS_GE65_MOE  = .[[40]]^2,
      TOT_DIS_TOT_EST  = .[[7]] + .[[21]] + .[[35]],	
      TOT_DIS_TOT_MOE  = .[[8]]^2 + .[[22]]^2 + .[[36]]^2,
      POV_DIS_TOT_EST  = .[[9]] + .[[23]] + .[[37]],	
      POV_DIS_TOT_MOE  = .[[10]]^2 + .[[24]]^2 + .[[38]]^2,
      POV_TOT_TOT_EST  = .[[9]] + .[[15]] + .[[23]] + .[[29]] + .[[37]] + .[[43]],	
      POV_TOT_TOT_MOE  = .[[10]]^2 + .[[16]]^2 + .[[24]]^2 + .[[30]]^2 + .[[38]]^2 + .[[44]]^2)  %>%
    select(GEOID, NAME, TOT_TOT_TOT_EST : POV_TOT_TOT_MOE)
  
  
  #  Colorado
  f.C18130_CO <- get_acs(geography = "state", 
                         table = 'C18130', 
                         survey = "acs5",
                         state ="CO",
                         year = curYr,
                         key = censAPI,
                         output ="wide") %>%
    mutate(
      GEOID = paste0(GEOID,"000"),
      TOT_TOT_TOT_EST  = .[[5]],	
      TOT_TOT_TOT_MOE  = .[[6]]^2,
      TOT_TOT_LE18_EST  = .[[7]],	
      TOT_TOT_LE18_MOE  = .[[8]]^2,
      TOT_DIS_LE18_EST  = .[[9]],	
      TOT_DIS_LE18_MOE  = .[[10]]^2,
      POV_DIS_LE18_EST  = .[[11]],	
      POV_DIS_LE18_MOE  = .[[12]]^2,
      TOT_TOT_1864_EST  = .[[21]],	
      TOT_TOT_1864_MOE  = .[[22]]^2,
      TOT_DIS_1864_EST  = .[[23]],	
      TOT_DIS_1864_MOE  = .[[24]]^2,
      POV_DIS_1864_EST  = .[[25]],	
      POV_DIS_1864_MOE  = .[[26]]^2,
      TOT_TOT_GE65_EST  = .[[35]],	
      TOT_TOT_GE65_MOE  = .[[36]]^2,
      TOT_DIS_GE65_EST  = .[[37]],	
      TOT_DIS_GE65_MOE  = .[[38]]^2,
      POV_DIS_GE65_EST  = .[[39]],	
      POV_DIS_GE65_MOE  = .[[40]]^2,
      TOT_DIS_TOT_EST  = .[[7]] + .[[21]] + .[[35]],	
      TOT_DIS_TOT_MOE  = .[[8]]^2 + .[[22]]^2 + .[[36]]^2,
      POV_DIS_TOT_EST  = .[[9]] + .[[23]] + .[[37]],	
      POV_DIS_TOT_MOE  = .[[10]]^2 + .[[24]]^2 + .[[38]]^2,
      POV_TOT_TOT_EST  = .[[9]] + .[[15]] + .[[23]] + .[[29]] + .[[37]] + .[[43]],	
      POV_TOT_TOT_MOE  = .[[10]]^2 + .[[16]]^2 + .[[24]]^2 + .[[30]]^2 + .[[38]]^2 + .[[44]]^2)  %>%
    select(GEOID, NAME, TOT_TOT_TOT_EST : POV_TOT_TOT_MOE)
  
  # POV_erty County
  f.C18130_cty<- get_acs(geography = "county", 
                         table = 'C18130', 
                         survey = "acs5",
                         state ="CO",
                         year = curYr,
                         key = censAPI,
                         output ="wide") %>%
    filter(GEOID %in% cty_str) %>%
    mutate(
      TOT_TOT_TOT_EST  = .[[5]],	
      TOT_TOT_TOT_MOE  = .[[6]]^2,
      TOT_TOT_LE18_EST  = .[[7]],	
      TOT_TOT_LE18_MOE  = .[[8]]^2,
      TOT_DIS_LE18_EST  = .[[9]],	
      TOT_DIS_LE18_MOE  = .[[10]]^2,
      POV_DIS_LE18_EST  = .[[11]],	
      POV_DIS_LE18_MOE  = .[[12]]^2,
      TOT_TOT_1864_EST  = .[[21]],	
      TOT_TOT_1864_MOE  = .[[22]]^2,
      TOT_DIS_1864_EST  = .[[23]],	
      TOT_DIS_1864_MOE  = .[[24]]^2,
      POV_DIS_1864_EST  = .[[25]],	
      POV_DIS_1864_MOE  = .[[26]]^2,
      TOT_TOT_GE65_EST  = .[[35]],	
      TOT_TOT_GE65_MOE  = .[[36]]^2,
      TOT_DIS_GE65_EST  = .[[37]],	
      TOT_DIS_GE65_MOE  = .[[38]]^2,
      POV_DIS_GE65_EST  = .[[39]],	
      POV_DIS_GE65_MOE  = .[[40]]^2,
      TOT_DIS_TOT_EST  = .[[7]] + .[[21]] + .[[35]],	
      TOT_DIS_TOT_MOE  = .[[8]]^2 + .[[22]]^2 + .[[36]]^2,
      POV_DIS_TOT_EST  = .[[9]] + .[[23]] + .[[37]],	
      POV_DIS_TOT_MOE  = .[[10]]^2 + .[[24]]^2 + .[[38]]^2,
      POV_TOT_TOT_EST  = .[[9]] + .[[15]] + .[[23]] + .[[29]] + .[[37]] + .[[43]],	
      POV_TOT_TOT_MOE  = .[[10]]^2 + .[[16]]^2 + .[[24]]^2 + .[[30]]^2 + .[[38]]^2 + .[[44]]^2)  %>%
    select(GEOID, NAME, TOT_TOT_TOT_EST : POV_TOT_TOT_MOE)
  
  if(length(cty_str) > 1) {  #Summarize for Multi County Place
    f.C18130_csbg <- f.C18130_cty %>%
      summarise(TOT_TOT_TOT_EST = sum(TOT_TOT_TOT_EST, na.rm=TRUE),
                TOT_TOT_LE18_EST = sum(TOT_TOT_LE18_EST, na.rm=TRUE),
                TOT_DIS_LE18_EST = sum(TOT_DIS_LE18_EST, na.rm=TRUE),
                POV_DIS_LE18_EST = sum(POV_DIS_LE18_EST, na.rm=TRUE),
                TOT_TOT_1864_EST = sum(TOT_TOT_1864_EST, na.rm=TRUE),
                TOT_DIS_1864_EST = sum(TOT_DIS_1864_EST, na.rm=TRUE),
                POV_DIS_1864_EST = sum(POV_DIS_1864_EST, na.rm=TRUE),
                TOT_TOT_GE65_EST = sum(TOT_TOT_GE65_EST, na.rm=TRUE),
                TOT_DIS_GE65_EST = sum(TOT_DIS_GE65_EST, na.rm=TRUE),
                POV_DIS_GE65_EST = sum(POV_DIS_GE65_EST, na.rm=TRUE),
                TOT_DIS_TOT_EST = sum(TOT_DIS_TOT_EST, na.rm=TRUE),
                POV_DIS_TOT_EST = sum(POV_DIS_TOT_EST, na.rm=TRUE),
                TOT_TOT_TOT_MOE = sum(TOT_TOT_TOT_MOE, na.rm=TRUE),
                TOT_TOT_LE18_MOE = sum(TOT_TOT_LE18_MOE, na.rm=TRUE),
                TOT_DIS_LE18_MOE = sum(TOT_DIS_LE18_MOE, na.rm=TRUE),
                POV_DIS_LE18_MOE = sum(POV_DIS_LE18_MOE, na.rm=TRUE),
                TOT_TOT_1864_MOE = sum(TOT_TOT_1864_MOE, na.rm=TRUE),
                TOT_DIS_1864_MOE = sum(TOT_DIS_1864_MOE, na.rm=TRUE),
                POV_DIS_1864_MOE = sum(POV_DIS_1864_MOE, na.rm=TRUE),
                TOT_TOT_GE65_MOE = sum(TOT_TOT_GE65_MOE, na.rm=TRUE),
                TOT_DIS_GE65_MOE = sum(TOT_DIS_GE65_MOE, na.rm=TRUE),
                POV_DIS_GE65_MOE = sum(POV_DIS_GE65_MOE, na.rm=TRUE),
                TOT_DIS_TOT_MOE   = sum(TOT_DIS_TOT_MOE, na.rm=TRUE),
                POV_DIS_TOT_MOE   = sum(POV_DIS_TOT_MOE, na.rm=TRUE),
                POV_TOT_TOT_EST  = sum(POV_TOT_TOT_EST, na.rm=TRUE),
                POV_TOT_TOT_MOE  = sum(POV_TOT_TOT_MOE, na.rm=TRUE)) %>%
      mutate(GEOID = "08000.5",
             NAME = name1) %>%
      select(GEOID, NAME, TOT_TOT_TOT_EST : POV_DIS_GE65_MOE)
    
    f.C18130_FIN <- bind_rows(f.C18130_US, f.C18130_CO, f.C18130_csbg, f.C18130_cty)
  } else {
    f.C18130_FIN <- bind_rows(f.C18130_US, f.C18130_CO, f.C18130_cty)
  }
  
  
  # calculating Percentage
  f.C18130_FIN <- f.C18130_FIN %>%
    mutate(NAME = str_replace(NAME, ", Colorado",""),
           TOT_TOT_TOT_MOE = sqrt(TOT_TOT_TOT_MOE),
           TOT_TOT_LE18_MOE = sqrt(TOT_TOT_LE18_MOE),
           TOT_DIS_LE18_MOE = sqrt(TOT_DIS_LE18_MOE),
           POV_DIS_LE18_MOE = sqrt(POV_DIS_LE18_MOE),
           TOT_TOT_1864_MOE = sqrt(TOT_TOT_1864_MOE),
           TOT_DIS_1864_MOE = sqrt(TOT_DIS_1864_MOE),
           POV_DIS_1864_MOE = sqrt(POV_DIS_1864_MOE),
           TOT_TOT_GE65_MOE = sqrt(TOT_TOT_GE65_MOE),
           TOT_DIS_GE65_MOE = sqrt(TOT_DIS_GE65_MOE),
           POV_DIS_GE65_MOE = sqrt(POV_DIS_GE65_MOE),
           TOT_DIS_TOT_MOE = sqrt(TOT_DIS_TOT_MOE),
           POV_DIS_TOT_MOE = sqrt(POV_DIS_TOT_MOE),
           POV_TOT_TOT_MOE  = sqrt(POV_TOT_TOT_MOE),
           
           TOT_DIS_LE18_EST_PCT = TOT_DIS_LE18_EST/TOT_DIS_TOT_EST,
           POV_DIS_LE18_EST_PCT = POV_DIS_LE18_EST/POV_DIS_TOT_EST,
           TOT_DIS_1864_EST_PCT = TOT_DIS_1864_EST/TOT_DIS_TOT_EST,
           POV_DIS_1864_EST_PCT = POV_DIS_1864_EST/POV_DIS_TOT_EST,
           TOT_DIS_GE65_EST_PCT = TOT_DIS_GE65_EST/TOT_DIS_TOT_EST,
           POV_DIS_GE65_EST_PCT = POV_DIS_GE65_EST/POV_DIS_TOT_EST,
           TOT_DIS_TOT_EST_PCT = TOT_DIS_TOT_EST/TOT_TOT_TOT_EST,
           POV_DIS_TOT_EST_PCT = POV_DIS_TOT_EST/POV_TOT_TOT_EST,
           TOT_DIS_LE18_MOE_PCT = pctMOE(POV_DIS_EST, POV_DIS_MOE,TOT_DIS_LE18_MOE, TOT_DIS_LE18_EST_PCT),
           POV_DIS_LE18_MOE_PCT = pctMOE(TOT_DIS_EST, TOT_DIS_MOE,POV_DIS_LE18_MOE, POV_DIS_LE18_EST_PCT),
           TOT_DIS_1864_MOE_PCT = pctMOE(POV_DIS_EST, POV_DIS_MOE,TOT_DIS_1864_MOE, TOT_DIS_1864_EST_PCT),
           POV_DIS_1864_MOE_PCT = pctMOE(TOT_DIS_EST, TOT_DIS_MOE,POV_DIS_1864_MOE, POV_DIS_1864_EST_PCT),
           TOT_DIS_GE65_MOE_PCT = pctMOE(POV_DIS_EST, POV_DIS_MOE,TOT_DIS_GE65_MOE, TOT_DIS_GE65_EST_PCT),
           POV_DIS_GE65_MOE_PCT = pctMOE(TOT_DIS_EST, TOT_DIS_MOE,POV_DIS_GE65_MOE, POV_DIS_GE65_EST_PCT),
           TOT_DIS_TOT_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE, TOT_DIS_TOT_MOE, TOT_DIS_TOT_EST_PCT),
           POV_DIS_TOT_EST_PCT = pctMOE(POV_TOT_TOT_EST,POV_TOT_TOT_MOE, POV_DIS_TOT_MOE, POV_DIS_TOT_EST_PCT)
    )  %>% 
    select(GEOID, NAME, TOT_TOT_TOT_EST : POV_DIS_TOT_EST_PCT)
  
 browser() 
  # Building Chart data sets
  f.dis_est <- f.C18130_FIN %>% select(GEOID, NAME, TOT_POV_EST_PCT, LTHSGRAD_POV_EST_PCT,	HSGRAD_POV_EST_PCT,	SCOLL_POV_EST_PCT,	BA_POV_EST_PCT,	LTHSGRAD_TOT_EST_PCT,	HSGRAD_TOT_EST_PCT,	SCOLL_TOT_EST_PCT,	BA_TOT_EST_PCT) %>%
    gather(dis_ATT, EST_PCT,TOT_POV_EST_PCT : BA_TOT_EST_PCT, factor_key =TRUE) %>%
    mutate(dis_ATT = str_replace(dis_ATT,"_EST_PCT",""))
  
  f.dis_moe <- f.C18130_FIN %>% select(GEOID, NAME, TOT_POV_MOE_PCT, LTHSGRAD_POV_MOE_PCT,	HSGRAD_POV_MOE_PCT,	SCOLL_POV_MOE_PCT,	BA_POV_MOE_PCT,	LTHSGRAD_TOT_MOE_PCT,	HSGRAD_TOT_MOE_PCT,	SCOLL_TOT_MOE_PCT,	BA_TOT_MOE_PCT) %>%
    gather(dis_ATT, MOE_PCT, TOT_POV_MOE_PCT : BA_TOT_MOE_PCT, factor_key =TRUE) %>%
    mutate(dis_ATT = str_replace(dis_ATT,"_MOE_PCT","")) %>% select(-NAME)
  
  
  f.dis_chart <- inner_join(f.dis_est, f.dis_moe, by =c('GEOID',"dis_ATT")) %>%
    mutate(EST_PCT = EST_PCT * 100,
           MOE_PCT = MOE_PCT * 100,
           TYPE = ifelse(grepl("_POV",dis_ATT),0,1),
           dis_ATT = str_replace(dis_ATT,"_POV",""),
           dis_ATT = str_replace(dis_ATT,"_TOT",""))
  f.dis_chart_tab <- f.dis_chart
  
  f.dis_chart <- f.dis_chart %>% filter(dis_ATT != "TOT")
  f.dis_chart$dis_ATT <- factor(f.dis_chart$dis_ATT, 
                                  levels = c("LTHSGRAD",  "HSGRAD", "SCOLL", "BA"),
                                  labels = c("Less than\nHigh School Graduate",
                                             "High School Graduate",
                                             "Some College/\nAssociate's Degree",
                                             "Bachelor's Degree or Higher"))
  
  f.dis_chart$TYPE <- factor(f.dis_chart$TYPE, levels=c(0,1),
                              labels =c("Below Federal Poverty Level",
                                        "Total Population"))
  
  nameList <- unique(f.dis_chart$NAME)
  f.dis_chart$NAME <- factor(f.dis_chart$NAME, levels =nameList)
  
  chart_list <- list()
  pltTitle <- paste0("disational Attianment by Poverty Status: ", name1)
  
  f.dis_chart3 <- f.dis_chart %>% filter(!(GEOID %in% cty_str))
  
  maxLim <- max(f.dis_chart$EST_PCT) + 10
  
  # Creating Bar Charts
  ggimg3 <-ggplot( f.dis_chart3, aes(x =as.factor(dis_ATT), y =EST_PCT, fill = NAME)) +
    geom_bar(stat ="identity", position = "dodge", color ="black") + 
    geom_text(aes(x =as.factor(dis_ATT), y =EST_PCT, label =percent(EST_PCT), group = NAME), 
              position = position_dodge(width =1.0),
              vjust =-0.8,  size =4) +
    scale_fill_manual(values =barCol) +
    scale_y_continuous(limits = c(0, maxLim), label =percent, expand = c(0, 0)) +
    facet_wrap(vars(TYPE), ncol=1) +
    labs(title = pltTitle,
         subtitle = "CSBG Comparison",
         caption = outCap,
         x = "disational Attainment",
         y = "Percentage",
         fill ="CSBG Entity") +
    theme(plot.title = element_text(hjust = 0.5, size =12),
          plot.caption = element_text(hjust = 0, size =9),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_blank(),
          axis.text.x = element_text(size =10),
          axis.text.y =element_text(size =10),
          legend.position = "bottom", legend.text=element_text(size=9))
  
  chart_list[[1]] <- ggimg3
  
  for(i in 1:length(cty_str)) {
    n <- i + 1
    f.chart_data <- f.dis_chart %>% filter(GEOID == "08000.5" | GEOID == cty_str[i]) 
    
    ggimg3 <-ggplot( f.chart_data, aes(x =as.factor(dis_ATT), y =EST_PCT, fill = NAME)) +
      geom_bar(stat ="identity", position = "dodge", color ="black") + 
      geom_text(aes(x =as.factor(dis_ATT), y =EST_PCT, label =percent(EST_PCT), group = NAME), 
                position = position_dodge(width =1.0),
                vjust =-0.8,  size =4) +
      scale_fill_manual(values =barCol2) +
      scale_y_continuous(limits = c(0, maxLim), label =percent, expand = c(0, 0)) +
      facet_wrap(vars(TYPE), ncol=1) +
      labs(title = pltTitle,
           subtitle = "County Comparison",
           caption = outCap,
           x = "disational Attainment",
           y = "Percentage",
           fill ="CSBG Entity") +
      theme(plot.title = element_text(hjust = 0.5, size =12),
            plot.caption = element_text(hjust = 0, size =9),
            panel.background = element_rect(fill = "white", colour = "gray50"),
            panel.grid.major = element_blank(),
            axis.text.x = element_text(size =10),
            axis.text.y =element_text(size =10),
            legend.position = "bottom", legend.text=element_text(size=9))
    chart_list[[n]] <- ggimg3
  }
  
  
  #Creating Table data file
  # selecting Total Povery Percentages
  f.dis_pov <- f.dis_chart_tab %>% filter(dis_ATT == "TOT") %>%
    mutate(EST_PCT_POV = EST_PCT/100,
           MOE_PCT_POV = MOE_PCT/100) %>%
    select(GEOID, EST_PCT_POV, MOE_PCT_POV)
  f.dis_tab <- inner_join(f.dis_chart_tab, f.age_fin, by ="GEOID") %>% 
    inner_join(., f.dis_pov, by="GEOID") %>%
    filter(dis_ATT != "TOT") %>%
    mutate(N_EST = ifelse(TYPE == 0, ((EST_PCT/100)*(EST_PCT_POV * totalpopulation)),(EST_PCT/100) * totalpopulation),
           N_MOE = ifelse(TYPE == 0, ((MOE_PCT/100)*(MOE_PCT_POV * totalpopulation)),(MOE_PCT/100) * totalpopulation)) %>%
    select(GEOID, NAME, year, TYPE, dis_ATT,N_EST, N_MOE, EST_PCT, MOE_PCT)
  
  f.dis_tab_tot <- f.dis_tab %>%
    group_by(GEOID, NAME, TYPE) %>%
    summarize(N_EST = sum(N_EST),
              N_MOE = sum(N_MOE),
              EST_PCT = sum(EST_PCT),
              MOE_PCT = sum(MOE_PCT)) %>%
    mutate(dis_ATT = "TOTAL",
           year = curYr) %>%
    select(GEOID, NAME, year, TYPE, dis_ATT, N_EST, N_MOE, EST_PCT, MOE_PCT)
  
  f.dis_tab_fin <- bind_rows(f.dis_tab, f.dis_tab_tot) %>%
    mutate(N_EST = comma_conv(N_EST),
           N_MOE = comma_conv(N_MOE),
           EST_PCT = percent(EST_PCT),
           MOE_PCT = percent(MOE_PCT))
  
  f.dis_tab_fin$dis_ATT <- factor(f.dis_tab_fin$dis_ATT, 
                                    levels = c("LTHSGRAD",  "HSGRAD", "SCOLL", "BA", "TOTAL"))
  
  f.dis_tab_fin_l <- f.dis_tab_fin %>% arrange(GEOID, factor(dis_ATT)) %>%
    pivot_wider(names_from =dis_ATT, values_from =c(N_EST, N_MOE, EST_PCT, MOE_PCT)) %>%
    arrange(GEOID) %>%
    mutate(NAME = ifelse(TYPE == 1,"",NAME)) %>%
    select("NAME", "TYPE",
           "N_EST_LTHSGRAD",	"N_MOE_LTHSGRAD",	"EST_PCT_LTHSGRAD",	"MOE_PCT_LTHSGRAD",
           "N_EST_HSGRAD",	"N_MOE_HSGRAD",	"EST_PCT_HSGRAD",	"MOE_PCT_HSGRAD",
           "N_EST_SCOLL",	"N_MOE_SCOLL",	"EST_PCT_SCOLL",	"MOE_PCT_SCOLL",
           "N_EST_BA",	"N_MOE_BA",	"EST_PCT_BA",	"MOE_PCT_BA",
           "N_EST_TOTAL",	"N_MOE_TOTAL",	"EST_PCT_TOTAL",	"MOE_PCT_TOTAL")
  
  f.dis_tab_fin_l$TYPE <- factor(f.dis_tab_fin_l$TYPE, levels=c(0,1),
                                  labels =c("Below Federal Poverty Level",
                                            "Total Population"))
  
  # Flex Table
  tab_head <- paste0("disational Attainment by Poverty Level, ",listID$plName1)
  head_level <- c("","","Less Than High School Graduate","","","",
                  "High School Graduate","","","",
                  "Some College/Associate's Degree","","","",
                  "Bachelor's Degree or Higher","","","",
                  "Total","","","")
  head_measure <- c("CSBG Entity","Type",rep(c("Estimate","Margin of Error"),10))
  
  f.disFlex <- flextable(
    f.dis_tab_fin_l,
    col_keys = names(f.dis_tab_fin_l)) %>%
    add_header_row(values =head_measure,top =TRUE) %>%
    add_header_row(values =head_level,top =TRUE) %>%
    align(j =1:22,align ="center",part ="header") %>%
    delete_rows(i = 3, part = "header") %>%
    merge_at(i =1,j =3:6, part ="header") %>%
    merge_at(i =1,j =7:10, part ="header") %>%
    merge_at(i =1,j =11:14, part ="header") %>%
    merge_at(i =1,j =15:18, part ="header") %>%
    merge_at(i =1,j =19:22, part ="header") %>%
    align(j =1:2, align ="left", part ="body") %>%
    align(j =3:22, align ="right", part ="body") %>%
    width(j =1:2, width =4) %>%
    width(j =3:22,width =1) 
  
  f.disFlex <- add_header_lines(f.disFlex,values =tab_head, top =TRUE)
  
  
  
  #bind list
  outList <- list("plot" = chart_list, "data" =  f.dis_tab_fin_l, "FlexTable" = f.disFlex)
  
  return(outList)
}
