#' insurance generates tables and plotly charts for Health insurance by age 
#' Using SAIHE data https://www.census.gov/programs-surveys/sahie.html

#'  CSBG Dashboard 11/2019  Revised 9/2022 A. Bickford
#' @param DBPool the DOLA database pool
#' @lvl the selected agency
#' @param listID is the list of selected county codes
#' @param cyrYr Current year value
#' @return plotly graphic, data table and data file
#' @export

insurance <- function(listID, curYr, inData, barCol, barCol2){
 
  
  # Collecting List of Counties
  outCap <- captionSrc("ACS",curYr,"C27016")
  cty_str <- listID$list2
  full_str <- c("01000","08000",unlist(cty_str))
  cty_str2 <- c("08000",unlist(cty_str))
  cty_name <- CountyName(cty_str)
  cty_num <- as.numeric(str_sub(cty_str,3,5))
  name1 <- listID$plName1
  
  
  # Building the data files  
  # Population Estimates age LT05, 0517, 1824, 2544,4564, GE65
  
  # Collecting US Data 
 

  f.USage_grp <- inData[["US"]] %>%  filter(AGE != 999) %>% 
    filter(SEX == 0) %>%
    mutate(age_group =  ifelse(AGE <= 18, "AGELE18_EST",
                                     ifelse(AGE <= 64, "AGE1964_EST", "AGEGE65_EST"))) %>%
    group_by(GEOID, age_group) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    mutate(countyname = "United States") %>%
    spread(age_group, totalpopulation) %>%
    mutate(TOTAL_EST = AGELE18_EST + AGE1964_EST + AGEGE65_EST ) %>%
    select(GEOID, countyname,
           AGELE18_EST, AGE1964_EST, AGEGE65_EST, TOTAL_EST) 
  

  f.COAge_long <- inData[["CO"]] %>% filter(GEOID %in% cty_str2) %>%
    mutate(age_group = ifelse(age <= 18,"AGELE18_EST",
                       ifelse(age <= 64, "AGE1964_EST", "AGEGE65_EST")))  %>%
    group_by(GEOID, countyname, age_group) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    spread(age_group, totalpopulation) %>%
    mutate(TOTAL_EST =  AGELE18_EST + AGE1964_EST + AGEGE65_EST ) %>%
    select(GEOID, countyname,
           AGELE18_EST, AGE1964_EST, AGEGE65_EST, TOTAL_EST) 
  
  
  if(length(listID$list2) > 1){  #Summing up CSBG Entity
    f.csbg_age <- f.COAge_long %>% filter(GEOID %in% cty_str) %>%
      mutate(year = 1) %>%
      group_by(year) %>%
      summarize(AGELE18_EST = sum(AGELE18_EST),
                AGE1964_EST = sum(AGE1964_EST),                
                AGEGE65_EST = sum(AGEGE65_EST),                
                TOTAL_EST = sum(TOTAL_EST))  %>%
      mutate(countyname = listID$plName1,
             GEOID = "08000.5") %>%
      select(GEOID, countyname,
             AGELE18_EST, AGE1964_EST, AGEGE65_EST, TOTAL_EST) 
    
    f.state_age <- f.COAge_long %>% filter(GEOID == "08000")
    f.cty_age  <- f.COAge_long %>% filter(GEOID %in% cty_str) 
    f.age_sya <- bind_rows(f.USage_grp, f.state_age, f.csbg_age, f.cty_age) 
  } else {
    f.age_sya <- bind_rows(f.USage_grp, f.COAge_long) 
  }
  
  # POVerty US
  f.C27016_ACS <- inData[["C27016"]] %>% filter(GEOID %in% full_str) %>%
    mutate(
      TOT_TOT_TOT_EST = .[[3]],	
      TOT_TOT_TOT_MOE = .[[4]]^2,
      POV_TOT_TOT_EST = .[[5]],	
      POV_TOT_TOT_MOE = .[[6]]^2,
      POV_LE18_TOT_EST = .[[7]],	
      POV_LE18_TOT_MOE = .[[8]]^2,
      POV_LE18_INS_EST = .[[9]],	
      POV_LE18_INS_MOE = .[[10]]^2,
      POV_LE18_NOINS_EST = .[[11]],	
      POV_LE18_NOINS_MOE = .[[12]]^2,
      POV_1964_TOT_EST = .[[13]],	
      POV_1964_TOT_MOE = .[[14]]^2,
      POV_1964_INS_EST = .[[15]],	
      POV_1964_INS_MOE = .[[16]]^2,
      POV_1964_NOINS_EST = .[[17]],	
      POV_1964_NOINS_MOE = .[[18]]^2,
      POV_GE65_TOT_EST = .[[19]],	
      POV_GE65_TOT_MOE = .[[20]]^2,
      POV_GE65_INS_EST = .[[21]],	
      POV_GE65_INS_MOE = .[[22]]^2,
      POV_GE65_NOINS_EST = .[[23]],	
      POV_GE65_NOINS_MOE = .[[24]]^2,
      POV_TOT_INS_EST = .[[9]] + .[[15]] + .[[21]],	
      POV_TOT_INS_MOE = .[[10]]^2 + .[[16]]^2 + .[[22]]^2,
      POV_TOT_NOINS_EST = .[[11]] + .[[17]] + .[[23]],	
      POV_TOT_NOINS_MOE = .[[12]]^2 + .[[18]]^2 + .[[24]]^2,
      
      
      TOT_LE18_TOT_EST = .[[7]] + .[[27]] + .[[47]] + .[[67]] + .[[87]],	
      TOT_LE18_TOT_MOE = .[[8]]^2 + .[[28]]^2 + .[[48]]^2 + .[[68]]^2 + .[[88]]^2,
      TOT_LE18_INS_EST = .[[9]] + .[[29]] + .[[49]] + .[[69]] + .[[89]],	
      TOT_LE18_INS_MOE = .[[10]]^2 + .[[30]]^2 + .[[50]]^2 + .[[70]]^2 + .[[90]]^2,
      TOT_LE18_NOINS_EST = .[[11]] + .[[31]] + .[[51]] + .[[71]] + .[[91]],	
      TOT_LE18_NOINS_MOE = .[[12]]^2 + .[[32]]^2 + .[[52]]^2 + .[[72]]^2 + .[[92]]^2,
      TOT_1964_TOT_EST = .[[13]] + .[[33]] + .[[53]] + .[[73]] + .[[93]],	
      TOT_1964_TOT_MOE = .[[14]]^2 + .[[34]]^2 + .[[54]]^2 + .[[74]]^2 + .[[94]]^2,
      TOT_1964_INS_EST = .[[15]] + .[[35]] + .[[55]] + .[[75]] + .[[95]],	
      TOT_1964_INS_MOE = .[[16]]^2 + .[[36]]^2 + .[[56]]^2 + .[[76]]^2 + .[[96]]^2,
      TOT_1964_NOINS_EST = .[[17]] + .[[37]] + .[[57]] + .[[77]] + .[[97]],	
      TOT_1964_NOINS_MOE = .[[18]]^2 + .[[38]]^2 + .[[58]]^2 + .[[78]]^2 + .[[98]]^2,
      TOT_GE65_TOT_EST = .[[19]] + .[[39]] + .[[59]] + .[[79]] + .[[99]],	
      TOT_GE65_TOT_MOE = .[[20]]^2 + .[[40]]^2 + .[[60]]^2 + .[[80]]^2 + .[[100]]^2,
      TOT_GE65_INS_EST = .[[21]] + .[[41]] + .[[61]] + .[[81]] + .[[101]],	
      TOT_GE65_INS_MOE = .[[22]]^2 + .[[42]]^2 + .[[62]]^2 + .[[82]]^2 + .[[102]]^2,
      TOT_GE65_NOINS_EST = .[[23]] + .[[43]] + .[[63]] + .[[83]] + .[[103]],	
      TOT_GE65_NOINS_MOE = .[[24]]^2 + .[[44]]^2 + .[[64]]^2 + .[[84]]^2 + .[[104]]^2,
      TOT_TOT_INS_EST = .[[9]] + .[[15]] + .[[21]] + .[[29]] + .[[35]] + .[[41]] + .[[49]] + .[[55]] + .[[61]] + .[[69]] + .[[75]] + .[[81]] + .[[89]] + .[[95]] + .[[101]],	
      TOT_TOT_INS_MOE = .[[10]]^2 + .[[16]]^2 + .[[22]]^2  + .[[30]]^2 + .[[36]]^2 + .[[42]]^2 + .[[50]]^2 + .[[56]]^2 + .[[62]]^2 + .[[70]]^2 + .[[76]]^2 + .[[82]]^2 + .[[90]]^2 + .[[96]]^2 + .[[102]]^2,
      TOT_TOT_NOINS_EST = .[[11]] + .[[17]] + .[[23]] + .[[31]] + .[[37]] + .[[43]] + .[[51]] + .[[57]] + .[[63]] + .[[71]] + .[[77]] + .[[83]] + .[[91]] + .[[97]] + .[[103]],	
      TOT_TOT_NOINS_MOE = .[[12]]^2 + .[[18]]^2 + .[[24]]^2  + .[[32]]^2 + .[[38]]^2 + .[[44]]^2 + .[[52]]^2 + .[[58]]^2 + .[[64]]^2 + .[[72]]^2 + .[[78]]^2 + .[[84]]^2 + .[[92]]^2 + .[[98]]^2 + .[[104]]^2
    )  %>%
    select(GEOID, NAME, TOT_TOT_TOT_EST, TOT_TOT_TOT_MOE, TOT_LE18_TOT_EST : TOT_TOT_NOINS_MOE,
           POV_TOT_TOT_EST : POV_TOT_NOINS_MOE)
  
  
  
  if(length(cty_str) > 1) {  #Summarize for Multi County Place
    f.C27016_csbg <- f.C27016_ACS %>% filter(GEOID %in% cty_str) %>%
      summarise(TOT_TOT_TOT_EST = sum(TOT_TOT_TOT_EST, na.rm=TRUE),	
                TOT_TOT_TOT_MOE = sum(TOT_TOT_TOT_MOE, na.rm=TRUE),
                POV_TOT_TOT_EST = sum(POV_TOT_TOT_EST, na.rm=TRUE),	
                POV_TOT_TOT_MOE = sum(POV_TOT_TOT_MOE, na.rm=TRUE),
                POV_LE18_TOT_EST = sum(POV_LE18_TOT_EST, na.rm=TRUE),	
                POV_LE18_TOT_MOE = sum(POV_LE18_TOT_MOE, na.rm=TRUE),
                POV_LE18_INS_EST = sum(POV_LE18_INS_EST, na.rm=TRUE),	
                POV_LE18_INS_MOE = sum(POV_LE18_INS_MOE, na.rm=TRUE),
                POV_LE18_NOINS_EST = sum(POV_LE18_NOINS_EST, na.rm=TRUE),	
                POV_LE18_NOINS_MOE = sum(POV_LE18_NOINS_MOE, na.rm=TRUE),
                POV_1964_TOT_EST = sum(POV_1964_TOT_EST, na.rm=TRUE),	
                POV_1964_TOT_MOE = sum(POV_1964_TOT_MOE, na.rm=TRUE),
                POV_1964_INS_EST = sum(POV_1964_INS_EST, na.rm=TRUE),	
                POV_1964_INS_MOE = sum(POV_1964_INS_MOE, na.rm=TRUE),
                POV_1964_NOINS_EST = sum(POV_1964_NOINS_EST, na.rm=TRUE),	
                POV_1964_NOINS_MOE = sum(POV_1964_NOINS_MOE, na.rm=TRUE),
                POV_GE65_TOT_EST = sum(POV_GE65_TOT_EST, na.rm=TRUE),	
                POV_GE65_TOT_MOE = sum(POV_GE65_TOT_MOE, na.rm=TRUE),
                POV_GE65_INS_EST = sum(POV_GE65_INS_EST, na.rm=TRUE),	
                POV_GE65_INS_MOE = sum(POV_GE65_INS_MOE, na.rm=TRUE),
                POV_GE65_NOINS_EST = sum(POV_GE65_NOINS_EST, na.rm=TRUE),	
                POV_GE65_NOINS_MOE = sum(POV_GE65_NOINS_MOE, na.rm=TRUE),
                POV_TOT_INS_EST = sum(POV_TOT_INS_EST, na.rm=TRUE),	
                POV_TOT_INS_MOE = sum(POV_TOT_INS_MOE, na.rm=TRUE),
                POV_TOT_NOINS_EST = sum(POV_TOT_NOINS_EST, na.rm=TRUE),	
                POV_TOT_NOINS_MOE = sum(POV_TOT_NOINS_MOE, na.rm=TRUE),

                TOT_LE18_TOT_EST = sum(TOT_LE18_TOT_EST, na.rm=TRUE),	
                TOT_LE18_TOT_MOE = sum(TOT_LE18_TOT_MOE, na.rm=TRUE),
                TOT_LE18_INS_EST = sum(TOT_LE18_INS_EST, na.rm=TRUE),	
                TOT_LE18_INS_MOE = sum(TOT_LE18_INS_MOE, na.rm=TRUE),
                TOT_LE18_NOINS_EST = sum(TOT_LE18_NOINS_EST, na.rm=TRUE),	
                TOT_LE18_NOINS_MOE = sum(TOT_LE18_NOINS_MOE, na.rm=TRUE),
                TOT_1964_TOT_EST = sum(TOT_1964_TOT_EST, na.rm=TRUE),	
                TOT_1964_TOT_MOE = sum(TOT_1964_TOT_MOE, na.rm=TRUE),
                TOT_1964_INS_EST = sum(TOT_1964_INS_EST, na.rm=TRUE),	
                TOT_1964_INS_MOE = sum(TOT_1964_INS_MOE, na.rm=TRUE),
                TOT_1964_NOINS_EST = sum(TOT_1964_NOINS_EST, na.rm=TRUE),	
                TOT_1964_NOINS_MOE = sum(TOT_1964_NOINS_MOE, na.rm=TRUE),
                TOT_GE65_TOT_EST = sum(TOT_GE65_TOT_EST, na.rm=TRUE),	
                TOT_GE65_TOT_MOE = sum(TOT_GE65_TOT_MOE, na.rm=TRUE),
                TOT_GE65_INS_EST = sum(TOT_GE65_INS_EST, na.rm=TRUE),	
                TOT_GE65_INS_MOE = sum(TOT_GE65_INS_MOE, na.rm=TRUE),
                TOT_GE65_NOINS_EST = sum(TOT_GE65_NOINS_EST, na.rm=TRUE),	
                TOT_GE65_NOINS_MOE = sum(TOT_GE65_NOINS_MOE, na.rm=TRUE),
                TOT_TOT_INS_EST = sum(TOT_TOT_INS_EST, na.rm=TRUE),	
                TOT_TOT_INS_MOE = sum(TOT_TOT_INS_MOE, na.rm=TRUE),
                TOT_TOT_NOINS_EST = sum(TOT_TOT_NOINS_EST, na.rm=TRUE),	
                TOT_TOT_NOINS_MOE = sum(TOT_TOT_NOINS_MOE, na.rm=TRUE)
      ) %>%
      mutate(GEOID = "08000.5",
             NAME = name1) %>%
      select(GEOID, NAME, TOT_TOT_TOT_EST, TOT_TOT_TOT_MOE, TOT_LE18_TOT_EST : TOT_TOT_NOINS_MOE,
             POV_TOT_TOT_EST : POV_TOT_NOINS_MOE)
    
    f.C27016_US <- f.C27016_ACS %>% filter(GEOID == "01000")
    f.C27016_CO <- f.C27016_ACS %>% filter(GEOID == "08000")
    f.C27016_CTY <- f.C27016_ACS %>% filter(GEOID %in% cty_str)
    
    f.C27016_FIN <- bind_rows(f.C27016_US, f.C27016_CO, f.C27016_csbg, f.C27016_CTY)
  } else {
    f.C27016_FIN <- f.C27016_ACS %>% filter(GEOID %in% full_str)
  }
  
  # calculating Percentage
  f.C27016_FIN <- f.C27016_FIN %>%
    mutate(NAME = str_replace(NAME, ", Colorado",""),
           TOT_TOT_TOT_MOE  = sqrt(TOT_TOT_TOT_MOE),
           POV_TOT_TOT_MOE  = sqrt(POV_TOT_TOT_MOE),
           POV_LE18_TOT_MOE  = sqrt(POV_LE18_TOT_MOE),
           POV_LE18_INS_MOE  = sqrt(POV_LE18_INS_MOE),
           POV_LE18_NOINS_MOE  = sqrt(POV_LE18_NOINS_MOE),
           POV_1964_TOT_MOE  = sqrt(POV_1964_TOT_MOE),
           POV_1964_INS_MOE  = sqrt(POV_1964_INS_MOE),
           POV_1964_NOINS_MOE  = sqrt(POV_1964_NOINS_MOE),
           POV_GE65_TOT_MOE  = sqrt(POV_GE65_TOT_MOE),
           POV_GE65_INS_MOE  = sqrt(POV_GE65_INS_MOE),
           POV_GE65_NOINS_MOE  = sqrt(POV_GE65_NOINS_MOE),
           POV_TOT_INS_MOE  = sqrt(POV_TOT_INS_MOE),
           POV_TOT_NOINS_MOE  = sqrt(POV_TOT_NOINS_MOE),
           
           
           TOT_LE18_TOT_MOE  = sqrt(TOT_LE18_TOT_MOE),
           TOT_LE18_INS_MOE  = sqrt(TOT_LE18_INS_MOE),
           TOT_LE18_NOINS_MOE  = sqrt(TOT_LE18_NOINS_MOE),
           TOT_1964_TOT_MOE  = sqrt(TOT_1964_TOT_MOE),
           TOT_1964_INS_MOE  = sqrt(TOT_1964_INS_MOE),
           TOT_1964_NOINS_MOE  = sqrt(TOT_1964_NOINS_MOE),
           TOT_GE65_TOT_MOE  = sqrt(TOT_GE65_TOT_MOE),
           TOT_GE65_INS_MOE  = sqrt(TOT_GE65_INS_MOE),
           TOT_GE65_NOINS_MOE  = sqrt(TOT_GE65_NOINS_MOE),
           TOT_TOT_INS_MOE  = sqrt(TOT_TOT_INS_MOE),
           TOT_TOT_NOINS_MOE  = sqrt(TOT_TOT_NOINS_MOE),
           
           POV_TOT_TOT_EST_PCT = POV_TOT_TOT_EST/TOT_TOT_TOT_EST,
           POV_TOT_TOT_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,POV_TOT_TOT_MOE, POV_TOT_TOT_EST_PCT),
           POV_LE18_INS_EST_PCT = POV_LE18_INS_EST/POV_LE18_TOT_EST,
           POV_LE18_INS_MOE_PCT = pctMOE(POV_LE18_TOT_EST,POV_LE18_TOT_MOE,POV_LE18_INS_MOE,POV_LE18_INS_EST_PCT),
           POV_LE18_NOINS_EST_PCT = POV_LE18_NOINS_EST/POV_LE18_TOT_EST,
           POV_LE18_NOINS_MOE_PCT = pctMOE(POV_LE18_INS_EST,POV_LE18_INS_MOE,POV_LE18_NOINS_MOE,POV_LE18_NOINS_EST_PCT),
           POV_LE18_TOT_EST_PCT = POV_LE18_TOT_EST/POV_TOT_TOT_EST,
           POV_LE18_TOT_MOE_PCT = pctMOE(POV_TOT_TOT_EST,POV_TOT_TOT_MOE,POV_LE18_TOT_MOE,POV_LE18_TOT_EST_PCT),
           
           POV_1964_INS_EST_PCT = POV_1964_INS_EST/POV_1964_TOT_EST,
           POV_1964_INS_MOE_PCT = pctMOE(POV_1964_TOT_EST,POV_1964_TOT_MOE,POV_1964_INS_MOE,POV_1964_INS_EST_PCT),
           POV_1964_NOINS_EST_PCT = POV_1964_NOINS_EST/POV_1964_TOT_EST,
           POV_1964_NOINS_MOE_PCT = pctMOE(POV_1964_INS_EST,POV_1964_INS_MOE,POV_1964_NOINS_MOE,POV_1964_NOINS_EST_PCT),
           POV_1964_TOT_EST_PCT = POV_1964_TOT_EST/POV_TOT_TOT_EST,
           POV_1964_TOT_MOE_PCT = pctMOE(POV_TOT_TOT_EST,POV_TOT_TOT_MOE,POV_1964_TOT_MOE,POV_1964_TOT_EST_PCT),
           
           POV_GE65_INS_EST_PCT = POV_GE65_INS_EST/POV_GE65_TOT_EST,
           POV_GE65_INS_MOE_PCT = pctMOE(POV_GE65_TOT_EST,POV_GE65_TOT_MOE,POV_GE65_INS_MOE,POV_GE65_INS_EST_PCT),
           POV_GE65_NOINS_EST_PCT = POV_GE65_NOINS_EST/POV_GE65_TOT_EST,
           POV_GE65_NOINS_MOE_PCT = pctMOE(POV_GE65_INS_EST,POV_GE65_INS_MOE,POV_GE65_NOINS_MOE,POV_GE65_NOINS_EST_PCT),
           POV_GE65_INS_EST_PCT = POV_GE65_INS_EST/POV_GE65_TOT_EST,
           POV_GE65_INS_MOE_PCT = pctMOE(POV_GE65_TOT_EST,POV_GE65_TOT_MOE,POV_GE65_INS_MOE,POV_GE65_INS_EST_PCT),
           POV_GE65_NOINS_EST_PCT = POV_GE65_NOINS_EST/POV_GE65_TOT_EST,
           POV_GE65_NOINS_MOE_PCT = pctMOE(POV_GE65_INS_EST,POV_GE65_INS_MOE,POV_GE65_NOINS_MOE,POV_GE65_NOINS_EST_PCT),
           POV_GE65_TOT_EST_PCT = POV_GE65_TOT_EST/POV_TOT_TOT_EST,
           POV_GE65_TOT_MOE_PCT = pctMOE(POV_TOT_TOT_EST,POV_TOT_TOT_MOE,POV_GE65_TOT_MOE,POV_GE65_TOT_EST_PCT),
           
           POV_TOT_INS_EST_PCT = POV_TOT_INS_EST/POV_TOT_TOT_EST,
           POV_TOT_INS_MOE_PCT = pctMOE(POV_TOT_TOT_EST,POV_TOT_TOT_MOE,POV_TOT_INS_MOE,POV_TOT_INS_EST_PCT),
           POV_TOT_NOINS_EST_PCT = POV_TOT_NOINS_EST/POV_TOT_TOT_EST,
           POV_TOT_NOINS_MOE_PCT = pctMOE(POV_TOT_INS_EST,POV_TOT_INS_MOE,POV_TOT_NOINS_MOE,POV_TOT_NOINS_EST_PCT),
           POV_TOT_TOT_EST_PCT = POV_TOT_TOT_EST/TOT_TOT_TOT_EST,
           POV_TOT_TOT_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,POV_TOT_TOT_MOE,POV_TOT_TOT_EST_PCT),

           TOT_TOT_TOT_EST_PCT = TOT_TOT_TOT_EST/TOT_TOT_TOT_EST,
           TOT_TOT_TOT_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,TOT_TOT_TOT_MOE, TOT_TOT_TOT_EST_PCT),
           TOT_LE18_INS_EST_PCT = TOT_LE18_INS_EST/TOT_LE18_TOT_EST,
           TOT_LE18_INS_MOE_PCT = pctMOE(TOT_LE18_TOT_EST,TOT_LE18_TOT_MOE,TOT_LE18_INS_MOE,TOT_LE18_INS_EST_PCT),
           TOT_LE18_NOINS_EST_PCT = TOT_LE18_NOINS_EST/TOT_LE18_TOT_EST,
           TOT_LE18_NOINS_MOE_PCT = pctMOE(TOT_LE18_INS_EST,TOT_LE18_INS_MOE,TOT_LE18_NOINS_MOE,TOT_LE18_NOINS_EST_PCT),
           TOT_LE18_TOT_EST_PCT = TOT_LE18_TOT_EST/TOT_TOT_TOT_EST,
           TOT_LE18_TOT_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,TOT_LE18_TOT_MOE,TOT_LE18_TOT_EST_PCT),
           
           TOT_1964_INS_EST_PCT = TOT_1964_INS_EST/TOT_1964_TOT_EST,
           TOT_1964_INS_MOE_PCT = pctMOE(TOT_1964_TOT_EST,TOT_1964_TOT_MOE,TOT_1964_INS_MOE,TOT_1964_INS_EST_PCT),
           TOT_1964_NOINS_EST_PCT = TOT_1964_NOINS_EST/TOT_1964_TOT_EST,
           TOT_1964_NOINS_MOE_PCT = pctMOE(TOT_1964_INS_EST,TOT_1964_INS_MOE,TOT_1964_NOINS_MOE,TOT_1964_NOINS_EST_PCT),
           TOT_1964_TOT_EST_PCT = TOT_1964_TOT_EST/TOT_TOT_TOT_EST,
           TOT_1964_TOT_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,TOT_1964_TOT_MOE,TOT_1964_TOT_EST_PCT),
           
           TOT_GE65_INS_EST_PCT = TOT_GE65_INS_EST/TOT_GE65_TOT_EST,
           TOT_GE65_INS_MOE_PCT = pctMOE(TOT_GE65_TOT_EST,TOT_GE65_TOT_MOE,TOT_GE65_INS_MOE,TOT_GE65_INS_EST_PCT),
           TOT_GE65_NOINS_EST_PCT = TOT_GE65_NOINS_EST/TOT_GE65_TOT_EST,
           TOT_GE65_NOINS_MOE_PCT = pctMOE(TOT_GE65_INS_EST,TOT_GE65_INS_MOE,TOT_GE65_NOINS_MOE,TOT_GE65_NOINS_EST_PCT),
           TOT_GE65_TOT_EST_PCT = TOT_GE65_TOT_EST/TOT_TOT_TOT_EST,
           TOT_GE65_TOT_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,TOT_GE65_TOT_MOE,TOT_GE65_TOT_EST_PCT),
          
           TOT_TOT_INS_EST_PCT = TOT_TOT_INS_EST/TOT_TOT_TOT_EST,
           TOT_TOT_INS_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,TOT_TOT_INS_MOE,TOT_TOT_INS_EST_PCT),
           TOT_TOT_NOINS_EST_PCT = TOT_TOT_NOINS_EST/TOT_TOT_TOT_EST,
           TOT_TOT_NOINS_MOE_PCT = pctMOE(TOT_TOT_INS_EST,TOT_TOT_INS_MOE,TOT_TOT_NOINS_MOE,TOT_TOT_NOINS_EST_PCT)
    )  %>% 
    select(GEOID, NAME, 
           POV_TOT_TOT_EST,	POV_TOT_TOT_MOE,	POV_TOT_TOT_EST_PCT,	POV_TOT_TOT_MOE_PCT,
           POV_LE18_INS_EST,	POV_LE18_INS_MOE,	POV_LE18_INS_EST_PCT,	POV_LE18_INS_MOE_PCT,
           POV_LE18_NOINS_EST,	POV_LE18_NOINS_MOE,	POV_LE18_NOINS_EST_PCT,	POV_LE18_NOINS_MOE_PCT,
           POV_LE18_TOT_EST,	POV_LE18_TOT_MOE,	POV_LE18_TOT_EST_PCT,	POV_LE18_TOT_MOE_PCT,
           POV_1964_INS_EST,	POV_1964_INS_MOE,	POV_1964_INS_EST_PCT,	POV_1964_INS_MOE_PCT,
           POV_1964_NOINS_EST,	POV_1964_NOINS_MOE,	POV_1964_NOINS_EST_PCT,	POV_1964_NOINS_MOE_PCT,
           POV_1964_TOT_EST,	POV_1964_TOT_MOE,	POV_1964_TOT_EST_PCT,	POV_1964_TOT_MOE_PCT,
           POV_GE65_INS_EST,	POV_GE65_INS_MOE,	POV_GE65_INS_EST_PCT,	POV_GE65_INS_MOE_PCT,
           POV_GE65_NOINS_EST,	POV_GE65_NOINS_MOE,	POV_GE65_NOINS_EST_PCT,	POV_GE65_NOINS_MOE_PCT,
           POV_GE65_INS_EST,	POV_GE65_INS_MOE,	POV_GE65_INS_EST_PCT,	POV_GE65_INS_MOE_PCT,
           POV_GE65_NOINS_EST,	POV_GE65_NOINS_MOE,	POV_GE65_NOINS_EST_PCT,	POV_GE65_NOINS_MOE_PCT,
           POV_GE65_TOT_EST,	POV_GE65_TOT_MOE,	POV_GE65_TOT_EST_PCT,	POV_GE65_TOT_MOE_PCT,
           POV_TOT_INS_EST,	POV_TOT_INS_MOE,	POV_TOT_INS_EST_PCT,	POV_TOT_INS_MOE_PCT,
           POV_TOT_NOINS_EST,	POV_TOT_NOINS_MOE,	POV_TOT_NOINS_EST_PCT,	POV_TOT_NOINS_MOE_PCT,
           POV_TOT_TOT_EST,	POV_TOT_TOT_MOE,	POV_TOT_TOT_EST_PCT,	POV_TOT_TOT_MOE_PCT,
           TOT_TOT_TOT_EST,	TOT_TOT_TOT_MOE,	TOT_TOT_TOT_EST_PCT,	TOT_TOT_TOT_MOE_PCT,
           TOT_LE18_INS_EST,	TOT_LE18_INS_MOE,	TOT_LE18_INS_EST_PCT,	TOT_LE18_INS_MOE_PCT,
           TOT_LE18_NOINS_EST,	TOT_LE18_NOINS_MOE,	TOT_LE18_NOINS_EST_PCT,	TOT_LE18_NOINS_MOE_PCT,
           TOT_LE18_TOT_EST,	TOT_LE18_TOT_MOE,	TOT_LE18_TOT_EST_PCT,	TOT_LE18_TOT_MOE_PCT,
           TOT_1964_INS_EST,	TOT_1964_INS_MOE,	TOT_1964_INS_EST_PCT,	TOT_1964_INS_MOE_PCT,
           TOT_1964_NOINS_EST,	TOT_1964_NOINS_MOE,	TOT_1964_NOINS_EST_PCT,	TOT_1964_NOINS_MOE_PCT,
           TOT_1964_TOT_EST,	TOT_1964_TOT_MOE,	TOT_1964_TOT_EST_PCT,	TOT_1964_TOT_MOE_PCT,
           TOT_GE65_INS_EST,	TOT_GE65_INS_MOE,	TOT_GE65_INS_EST_PCT,	TOT_GE65_INS_MOE_PCT,
           TOT_GE65_NOINS_EST,	TOT_GE65_NOINS_MOE,	TOT_GE65_NOINS_EST_PCT,	TOT_GE65_NOINS_MOE_PCT,
           TOT_GE65_TOT_EST,	TOT_GE65_TOT_MOE,	TOT_GE65_TOT_EST_PCT,	TOT_GE65_TOT_MOE_PCT,
           TOT_TOT_INS_EST_PCT,	TOT_TOT_INS_MOE_PCT,	TOT_TOT_INS_EST_PCT,	TOT_TOT_INS_MOE_PCT,
           TOT_TOT_NOINS_EST_PCT,	TOT_TOT_NOINS_MOE_PCT,	TOT_TOT_NOINS_EST_PCT,	TOT_TOT_NOINS_MOE_PCT)


  # Building Chart data sets
  f.ins_est <- f.C27016_FIN %>% select(GEOID, NAME, 
                                        POV_LE18_INS_EST_PCT,	POV_LE18_NOINS_EST_PCT,	POV_1964_INS_EST_PCT,	POV_1964_NOINS_EST_PCT,	
                                        POV_GE65_INS_EST_PCT,	POV_GE65_NOINS_EST_PCT,	POV_TOT_INS_EST_PCT,	POV_TOT_NOINS_EST_PCT,
                                        TOT_LE18_INS_EST_PCT,	TOT_LE18_NOINS_EST_PCT,	TOT_1964_INS_EST_PCT,	TOT_1964_NOINS_EST_PCT,	
                                        TOT_GE65_INS_EST_PCT,	TOT_GE65_NOINS_EST_PCT, TOT_TOT_INS_EST_PCT,	TOT_TOT_NOINS_EST_PCT) %>%
    gather(INS_STAT, EST_PCT, POV_LE18_INS_EST_PCT :  TOT_TOT_NOINS_EST_PCT, factor_key =TRUE) %>%
    mutate(INS_STAT = str_replace(INS_STAT,"_EST_PCT",""))
  
  f.ins_moe <- f.C27016_FIN %>% select(GEOID, NAME, 
                                       POV_LE18_INS_MOE_PCT,	POV_LE18_NOINS_MOE_PCT,	POV_1964_INS_MOE_PCT,	POV_1964_NOINS_MOE_PCT,	
                                       POV_GE65_INS_MOE_PCT,	POV_GE65_NOINS_MOE_PCT,	POV_TOT_INS_MOE_PCT,	POV_TOT_NOINS_MOE_PCT,
                                       TOT_LE18_INS_MOE_PCT,	TOT_LE18_NOINS_MOE_PCT,	TOT_1964_INS_MOE_PCT,	TOT_1964_NOINS_MOE_PCT,	
                                       TOT_GE65_INS_MOE_PCT,	TOT_GE65_NOINS_MOE_PCT, TOT_TOT_INS_MOE_PCT,	TOT_TOT_NOINS_MOE_PCT) %>%
    gather(INS_STAT, MOE_PCT, POV_LE18_INS_MOE_PCT : TOT_TOT_NOINS_MOE_PCT, factor_key =TRUE) %>%
    mutate(INS_STAT = str_replace(INS_STAT,"_MOE_PCT","")) %>% select(-NAME)
  

  f.ins_chart <- inner_join(f.ins_est, f.ins_moe, by =c('GEOID',"INS_STAT"))   %>%
    mutate(EST_PCT = EST_PCT * 100,
          MOE_PCT = MOE_PCT * 100,
           TYPE = ifelse(grepl("POV", INS_STAT),0,1),
           INSURE = ifelse(grepl("NOINS", INS_STAT),1,0),
           INS_STAT = str_sub(INS_STAT,5,nchar(INS_STAT)),
           INS_STAT = str_replace(INS_STAT,"_NOINS",""),
           INS_STAT = str_replace(INS_STAT,"_INS","") 
           )  %>% replace(is.na(.),0)

  
  
  f.ins_chart$INSURE<- factor(f.ins_chart$INSURE, 
                                   levels = c(0,1),
                                   labels = c("Health Insurance",
                                              "No Health Insurance"))
  
  f.ins_chart$TYPE <- factor(f.ins_chart$TYPE, levels=c(0,1),
                              labels =c("Below Federal Poverty Level",
                                        "Total Population"))
  f.ins_chart$INS_STAT <- factor(f.ins_chart$INS_STAT, levels=c("LE18",	"1964",	"GE65", "TOT"),
                             labels =c("Age Less than 19",
                                       "Age 19 to 64",
                                       "Age 65 and Older",
                                       "Total"))
  
 
 
  chart_list <- list()
  pltTitle <- paste0("Health Insurance by Age and Poverty Level: ", name1)
  
  

  nameList <- unique(f.ins_chart$NAME)
  f.ins_chart$NAME <- factor(f.ins_chart$NAME, levels =nameList)
  
  chart_list <- list()
  pltTitle <- paste0("Health Insurance by Age and Poverty Status: ", name1)
  
  if(length(cty_str) > 1) {
    f.ins_chart3 <- f.ins_chart %>% filter(!(GEOID %in% cty_str))
  } else {
    f.ins_chart3 <- f.ins_chart
  }
  
  barCol6 <- c("#359A7E", "#7FFF00", "#6D3A5D", "#F08080", "#007ADE", "#ADD8E6")
  barCol4 <- c("#007ADE", "#ADD8E6", "#C0504D", "#F08080")
  

  # Creating Bar Charts
  ggimg3 <-ggplot(f.ins_chart3, aes(x =as.factor(INS_STAT), y =EST_PCT, fill = interaction(INSURE, NAME))) +
    geom_bar(stat ="identity", position = "dodge", color ="black") + 
    geom_text(aes(x =as.factor(INS_STAT), y =EST_PCT, label =percent(EST_PCT), group = interaction(INSURE, NAME)), 
              position = position_dodge(width =1.0),
              vjust =-0.8,  size = 2) +
    scale_fill_manual(values =barCol6) +
    scale_y_continuous(breaks = seq(0, 100, by=20), limits = c(0, 120), label =percent,  expand = c(0, 0)) +
    facet_wrap(vars(TYPE), ncol=1) +
    labs(title = pltTitle,
         subtitle = "CSBG Comparison",
         caption = outCap,
         x = "Age Group",
         y = "Percentage",
         fill ="Health Insurance\nCSBG Entity",
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
          legend.text = element_text(size=8)) #change legend text font siz
  
  chart_list[[1]] <- ggimg3

  if(length(cty_str) > 1){
    for(i in 1:length(cty_str)) {
      n <- i + 1
      f.chart_data <- f.ins_chart %>% filter(GEOID == "08000.5" | GEOID == cty_str[i]) 
      
      ggimg3 <-ggplot(  f.chart_data, aes(x =as.factor(INS_STAT), y =EST_PCT, fill = interaction(INSURE, NAME))) +
        geom_bar(stat ="identity", position = "dodge", color ="black") + 
        geom_text(aes(x =as.factor(INS_STAT), y =EST_PCT, label =percent(EST_PCT), group = interaction(INSURE, NAME)), 
                  position = position_dodge(width =1.0),
                  vjust =-0.8,  size = 2) +
        scale_fill_manual(values =barCol4) +
        scale_y_continuous(breaks = seq(0, 100, by=20), limits = c(0, 120), label =percent,  expand = c(0, 0)) +
        facet_wrap(vars(TYPE), ncol=1) +
        labs(title = pltTitle,
             subtitle = "County Comparison",
             caption = outCap,
             x = "Age Group",
             y = "Percentage",
             fill ="Health Insurance\nCSBG Entity",
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
  # Adjusting totals
  
  #Creating Table data file
  # Adjusting totals
  
  f.us_tab <- inData[["US"]] %>% filter(SEX == 0) %>% filter(AGE == 999) %>%
    select(GEOID, totalpopulation)
  
  
  f.st_tab <- inData[["CO"]] %>% filter(GEOID == "08000") %>%
    group_by(GEOID) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    select(GEOID, totalpopulation)
  
  f.cty_tab <- inData[["CO"]]  %>% filter(GEOID %in% cty_str) %>% 
    group_by(GEOID) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    select(GEOID, totalpopulation)
  
  if(length(cty_num) > 1) {  #Summarize for Multi County Place
    f.csbg_tab <- f.cty_tab %>%
      summarize(totalpopulation = sum(totalpopulation)) %>%
      mutate(GEOID = "08000.5") %>%
      select(GEOID, totalpopulation)
    
    f.ctypop_fin <- bind_rows(f.us_tab, f.st_tab, f.csbg_tab, f.cty_tab) 
  } else {  
    f.ctypop_fin <- bind_rows(f.us_tab, f.st_tab, f.cty_tab) 
  }
 
    f.C27016_ADJ <- inner_join(f.C27016_FIN, f.ctypop_fin, by="GEOID") %>%
       mutate(POV_TOT_TOT_EST_ADJ = POV_TOT_TOT_EST_PCT * totalpopulation,
              POV_TOT_TOT_MOE_ADJ = POV_TOT_TOT_MOE_PCT * totalpopulation,
              POV_LE18_INS_EST_ADJ = POV_LE18_INS_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_LE18_INS_MOE_ADJ = POV_LE18_INS_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_LE18_NOINS_EST_ADJ = POV_LE18_NOINS_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_LE18_NOINS_MOE_ADJ = POV_LE18_NOINS_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_LE18_TOT_EST_ADJ = POV_LE18_TOT_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_LE18_TOT_MOE_ADJ = POV_LE18_TOT_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_1964_INS_EST_ADJ = POV_1964_INS_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_1964_INS_MOE_ADJ = POV_1964_INS_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_1964_NOINS_EST_ADJ = POV_1964_NOINS_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_1964_NOINS_MOE_ADJ = POV_1964_NOINS_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_1964_TOT_EST_ADJ = POV_1964_TOT_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_1964_TOT_MOE_ADJ = POV_1964_TOT_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_GE65_INS_EST_ADJ = POV_GE65_INS_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_GE65_INS_MOE_ADJ = POV_GE65_INS_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_GE65_NOINS_EST_ADJ = POV_GE65_NOINS_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_GE65_NOINS_MOE_ADJ = POV_GE65_NOINS_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_GE65_TOT_EST_ADJ = POV_GE65_TOT_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_GE65_TOT_MOE_ADJ = POV_GE65_TOT_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_TOT_INS_EST_ADJ = POV_TOT_INS_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_TOT_INS_MOE_ADJ = POV_TOT_INS_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_TOT_NOINS_EST_ADJ = POV_TOT_NOINS_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_TOT_NOINS_MOE_ADJ = POV_TOT_NOINS_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              POV_TOT_TOT_EST_ADJ = POV_TOT_TOT_EST_PCT * POV_TOT_TOT_EST_ADJ,
              POV_TOT_TOT_MOE_ADJ = POV_TOT_TOT_MOE_PCT * POV_TOT_TOT_MOE_ADJ,
              
              TOT_LE18_INS_EST_ADJ = TOT_LE18_INS_EST_PCT * totalpopulation,
              TOT_LE18_INS_MOE_ADJ = TOT_LE18_INS_MOE_PCT * totalpopulation,
              TOT_LE18_NOINS_EST_ADJ = TOT_LE18_NOINS_EST_PCT * totalpopulation,
              TOT_LE18_NOINS_MOE_ADJ = TOT_LE18_NOINS_MOE_PCT * totalpopulation,
              TOT_LE18_TOT_EST_ADJ = TOT_LE18_TOT_EST_PCT * totalpopulation,
              TOT_LE18_TOT_MOE_ADJ = TOT_LE18_TOT_MOE_PCT * totalpopulation,
              
              TOT_1964_INS_EST_ADJ = TOT_1964_INS_EST_PCT * totalpopulation,
              TOT_1964_INS_MOE_ADJ = TOT_1964_INS_MOE_PCT * totalpopulation,
              TOT_1964_NOINS_EST_ADJ = TOT_1964_NOINS_EST_PCT * totalpopulation,
              TOT_1964_NOINS_MOE_ADJ = TOT_1964_NOINS_MOE_PCT * totalpopulation,
              TOT_1964_TOT_EST_ADJ = TOT_1964_TOT_EST_PCT * totalpopulation,
              TOT_1964_TOT_MOE_ADJ = TOT_1964_TOT_MOE_PCT * totalpopulation,
              
              TOT_GE65_INS_EST_ADJ = TOT_GE65_INS_EST_PCT * totalpopulation,
              TOT_GE65_INS_MOE_ADJ = TOT_GE65_INS_MOE_PCT * totalpopulation,
              TOT_GE65_NOINS_EST_ADJ = TOT_GE65_NOINS_EST_PCT * totalpopulation,
              TOT_GE65_NOINS_MOE_ADJ = TOT_GE65_NOINS_MOE_PCT * totalpopulation,
              TOT_GE65_TOT_EST_ADJ = TOT_GE65_TOT_EST_PCT * totalpopulation,
              TOT_GE65_TOT_MOE_ADJ = TOT_GE65_TOT_MOE_PCT * totalpopulation,
              
              TOT_TOT_INS_EST_ADJ = TOT_TOT_INS_EST_PCT * totalpopulation,
              TOT_TOT_INS_MOE_ADJ = TOT_TOT_INS_MOE_PCT * totalpopulation,
              TOT_TOT_NOINS_EST_ADJ = TOT_TOT_NOINS_EST_PCT * totalpopulation,
              TOT_TOT_NOINS_MOE_ADJ = TOT_TOT_NOINS_MOE_PCT * totalpopulation,
              TOT_TOT_TOT_EST_ADJ = totalpopulation,
              TOT_TOT_TOT_MOE_ADJ = 0
              )
    

    if(length(cty_name) > 1) {
      name_list <- unique(c("United States","Colorado",name1,cty_name))
    } else {
      name_list <- c("United States","Colorado", cty_name)
    }
    
    f.C27016_ADJ$NAME <- factor(f.C27016_ADJ$NAME ,levels=name_list)
    
    f.ins_pov <- f.C27016_ADJ %>% 
      mutate(TYPE = 0) %>%
      select(GEOID, NAME, TYPE, 
             POV_LE18_INS_EST_ADJ,	POV_LE18_INS_MOE_ADJ,	POV_LE18_INS_EST_PCT,	POV_LE18_INS_MOE_PCT,	POV_LE18_NOINS_EST_ADJ,	POV_LE18_NOINS_MOE_ADJ,	POV_LE18_NOINS_EST_PCT,	POV_LE18_NOINS_MOE_PCT,	POV_LE18_TOT_EST_ADJ,	POV_LE18_TOT_MOE_ADJ,
             POV_1964_INS_EST_ADJ,	POV_1964_INS_MOE_ADJ,	POV_1964_INS_EST_PCT,	POV_1964_INS_MOE_PCT,	POV_1964_NOINS_EST_ADJ,	POV_1964_NOINS_MOE_ADJ,	POV_1964_NOINS_EST_PCT,	POV_1964_NOINS_MOE_PCT,	POV_1964_TOT_EST_ADJ,	POV_1964_TOT_MOE_ADJ,
             POV_GE65_INS_EST_ADJ,	POV_GE65_INS_MOE_ADJ,	POV_GE65_INS_EST_PCT,	POV_GE65_INS_MOE_PCT,	POV_GE65_NOINS_EST_ADJ,	POV_GE65_NOINS_MOE_ADJ,	POV_GE65_NOINS_EST_PCT,	POV_GE65_NOINS_MOE_PCT,	POV_GE65_TOT_EST_ADJ,	POV_GE65_TOT_MOE_ADJ,
             POV_TOT_INS_EST_ADJ,	POV_TOT_INS_MOE_ADJ,	POV_TOT_INS_EST_PCT,	POV_TOT_INS_MOE_PCT,	POV_TOT_NOINS_EST_ADJ,	POV_TOT_NOINS_MOE_ADJ,	POV_TOT_NOINS_EST_PCT,	POV_TOT_NOINS_MOE_PCT,	POV_TOT_TOT_EST_ADJ,	POV_TOT_TOT_MOE_ADJ)
    names(f.ins_pov) <- c("GEOID", "NAME",	"TYPE",	
                          "LE18_INS_EST_ADJ",	"LE18_INS_MOE_ADJ",	"LE18_INS_EST_PCT",	"LE18_INS_MOE_PCT",	"LE18_NOINS_EST_ADJ",	"LE18_NOINS_MOE_ADJ",	"LE18_NOINS_EST_PCT",	"LE18_NOINS_MOE_PCT",	"LE18_TOT_EST_ADJ",	"LE18_TOT_MOE_ADJ",
                          "1964_INS_EST_ADJ",	"1964_INS_MOE_ADJ",	"1964_INS_EST_PCT",	"1964_INS_MOE_PCT",	"1964_NOINS_EST_ADJ",	"1964_NOINS_MOE_ADJ",	"1964_NOINS_EST_PCT",	"1964_NOINS_MOE_PCT",	"1964_TOT_EST_ADJ",	"1964_TOT_MOE_ADJ",
                          "GE65_INS_EST_ADJ",	"GE65_INS_MOE_ADJ",	"GE65_INS_EST_PCT",	"GE65_INS_MOE_PCT",	"GE65_NOINS_EST_ADJ",	"GE65_NOINS_MOE_ADJ",	"GE65_NOINS_EST_PCT",	"GE65_NOINS_MOE_PCT",	"GE65_TOT_EST_ADJ",	"GE65_TOT_MOE_ADJ",
                          "TOT_INS_EST_ADJ",	"TOT_INS_MOE_ADJ",	"TOT_INS_EST_PCT",	"TOT_INS_MOE_PCT",	"TOT_NOINS_EST_ADJ",	"TOT_NOINS_MOE_ADJ",	"TOT_NOINS_EST_PCT",	"TOT_NOINS_MOE_PCT",	"TOT_TOT_EST_ADJ",	"TOT_TOT_MOE_ADJ")
    
    
    f.ins_tot <- f.C27016_ADJ %>% 
      mutate(TYPE = 1) %>%
      select(GEOID, NAME, TYPE, 
             TOT_LE18_INS_EST_ADJ,	TOT_LE18_INS_MOE_ADJ,	TOT_LE18_INS_EST_PCT,	TOT_LE18_INS_MOE_PCT,	TOT_LE18_NOINS_EST_ADJ,	TOT_LE18_NOINS_MOE_ADJ,	TOT_LE18_NOINS_EST_PCT,	TOT_LE18_NOINS_MOE_PCT,	TOT_LE18_TOT_EST_ADJ,	TOT_LE18_TOT_MOE_ADJ,
             TOT_1964_INS_EST_ADJ,	TOT_1964_INS_MOE_ADJ,	TOT_1964_INS_EST_PCT,	TOT_1964_INS_MOE_PCT,	TOT_1964_NOINS_EST_ADJ,	TOT_1964_NOINS_MOE_ADJ,	TOT_1964_NOINS_EST_PCT,	TOT_1964_NOINS_MOE_PCT,	TOT_1964_TOT_EST_ADJ,	TOT_1964_TOT_MOE_ADJ,
             TOT_GE65_INS_EST_ADJ,	TOT_GE65_INS_MOE_ADJ,	TOT_GE65_INS_EST_PCT,	TOT_GE65_INS_MOE_PCT,	TOT_GE65_NOINS_EST_ADJ,	TOT_GE65_NOINS_MOE_ADJ,	TOT_GE65_NOINS_EST_PCT,	TOT_GE65_NOINS_MOE_PCT,	TOT_GE65_TOT_EST_ADJ,	TOT_GE65_TOT_MOE_ADJ,
             TOT_TOT_INS_EST_ADJ,	TOT_TOT_INS_MOE_ADJ,	TOT_TOT_INS_EST_PCT,	TOT_TOT_INS_MOE_PCT,	TOT_TOT_NOINS_EST_ADJ,	TOT_TOT_NOINS_MOE_ADJ,	TOT_TOT_NOINS_EST_PCT,	TOT_TOT_NOINS_MOE_PCT,	TOT_TOT_TOT_EST_ADJ,	TOT_TOT_TOT_MOE_ADJ
             )
    names(f.ins_tot) <- c("GEOID", "NAME",	"TYPE",	
                          "LE18_INS_EST_ADJ",	"LE18_INS_MOE_ADJ",	"LE18_INS_EST_PCT",	"LE18_INS_MOE_PCT",	"LE18_NOINS_EST_ADJ",	"LE18_NOINS_MOE_ADJ",	"LE18_NOINS_EST_PCT",	"LE18_NOINS_MOE_PCT",	"LE18_TOT_EST_ADJ",	"LE18_TOT_MOE_ADJ",
                          "1964_INS_EST_ADJ",	"1964_INS_MOE_ADJ",	"1964_INS_EST_PCT",	"1964_INS_MOE_PCT",	"1964_NOINS_EST_ADJ",	"1964_NOINS_MOE_ADJ",	"1964_NOINS_EST_PCT",	"1964_NOINS_MOE_PCT",	"1964_TOT_EST_ADJ",	"1964_TOT_MOE_ADJ",
                          "GE65_INS_EST_ADJ",	"GE65_INS_MOE_ADJ",	"GE65_INS_EST_PCT",	"GE65_INS_MOE_PCT",	"GE65_NOINS_EST_ADJ",	"GE65_NOINS_MOE_ADJ",	"GE65_NOINS_EST_PCT",	"GE65_NOINS_MOE_PCT",	"GE65_TOT_EST_ADJ",	"GE65_TOT_MOE_ADJ",
                          "TOT_INS_EST_ADJ",	"TOT_INS_MOE_ADJ",	"TOT_INS_EST_PCT",	"TOT_INS_MOE_PCT",	"TOT_NOINS_EST_ADJ",	"TOT_NOINS_MOE_ADJ",	"TOT_NOINS_EST_PCT",	"TOT_NOINS_MOE_PCT",	"TOT_TOT_EST_ADJ",	"TOT_TOT_MOE_ADJ")
    
    

    f.ins_tab_ADJ_l <- bind_rows(f.ins_pov, f.ins_tot) %>%
      arrange(NAME, TYPE) %>%
      select(-GEOID) 
    
    f.ins_tab_ADJ_l[,c(3,	4,	7,	8,	
                       11,	12,	13,	14,	
                       17,	18,	21,	22,	
                       23,	24,	27,	28,	
                       31,	32,	33,	34,	
                       37,	38,	41,	42)] <-
      sapply(f.ins_tab_ADJ_l[,c(3,	4,	7,	8,	
                                11,	12,	13,	14,	
                                17,	18,	21,	22,	
                                23,	24,	27,	28,	
                                31,	32,	33,	34,	
                                37,	38,	41,	42)], 
             function(x) comma_conv(x))
    f.ins_tab_ADJ_l[,c(5,	6,	9,	10,	15,	16,	
                       19,	20,	25,	26,	29,	
                       30,	35,	36,	39,	40)] <-
      sapply(f.ins_tab_ADJ_l[,c(5,	6,	9,	10,	15,	16,	
                                19,	20,	25,	26,	29,	
                                30,	35,	36,	39,	40)], function(x) percent(x * 100))
    
    f.ins_tab_ADJ_l$NAME <- as.character(f.ins_tab_ADJ_l$NAME)
    for(i in 1:nrow(f.ins_tab_ADJ_l)) {
      if(i %% 2 == 0) {
        f.ins_tab_ADJ_l[i,1] = ""
      }
    }
    
    f.ins_tab_ADJ_l$TYPE <- factor(f.ins_tab_ADJ_l$TYPE, levels=c(0,1),
                                    labels =c("Below Federal Poverty Level",
                                              "Total Population"))
    
    # Flex Table
    tab_head <- paste0("Health Insurance by Age and Poverty Status, ",listID$plName1)
    head_level1 <- c("",	"",	"Age Under 19",	"",	"",	"",	"",	"",	"",	"",	"",	"",	
                     "Age 19 to 64",	"",	"",	"",	"",	"",	"",	"",	"",	"",	
                     "Age 65 and Older",	"",	"",	"",	"",	"",	"",	"",	"",	"",	
                     "Total",	"",	"",	"",	"",	"",	"",	"",	"",	"")
    head_level2 <- c("",	"",	"Health Insurance",	"",	"",	"",	"No Health Insurance",	"",	"",	"",	"Total",	"",	
                     "Health Insurance",	"",	"",	"",	"No Health Insurance",	"",	"",	"",	"Total",	"",	
                     "Health Insurance",	"",	"",	"",	"No Health Insurance",	"",	"",	"",	"Total",	"",	
                     "Health Insurance",	"",	"",	"",	"No Health Insurance",	"",	"",	"",	"Total",	"")
    
    head_measure <- c("CSBG Entity","Poverty Status",rep(c("Estimate","Margin of Error"),20))
    headers <-t(bind_cols(as.data.frame(head_level1), as.data.frame(head_level2), as.data.frame(head_measure)))
    
    f.insFlex <- flextable(
      f.ins_tab_ADJ_l,
      col_keys = names(f.ins_tab_ADJ_l)) %>%
      add_header_row(values =head_measure,top =TRUE) %>%
      add_header_row(values =head_level2,top =TRUE) %>%
      add_header_row(values =head_level1,top =TRUE) %>%
      align(j =1:42,align ="center",part ="header") %>%
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
    
    f.insFlex <- add_header_lines(f.insFlex,values =tab_head, top =TRUE)

    #bind list
    outList <- list("plot" = chart_list, "data" =  f.ins_tab_ADJ_l, "header_sh"= headers, "FlexTable" = f.insFlex)
    
    return(outList)
  }
