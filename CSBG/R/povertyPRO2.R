#' povertyPRO2 generates tables from SAIPE county data for the current year
#'  CSBG Dashboard 11/2019  A. Bickford
#'  Pulling data from SAIPE API  
#' @param DBPool the DOLA database pool
#' @lvl the selected agency
#' @param listID is the list of selected county codes
#' @param ACS is the current ACS data file
#' @param cyrYr Current year value
#' @return plotly graphic, data table and data file
#' @export

povertyPRO2 <- function(listID,curYr, inData, barCol, barCol2){

  outCap <- captionSrc("ACS",curYr,"B17001")
  cty_str <- listID$list2
  full_str <- c("01000","08000",unlist(cty_str))
  cty_str2 <- c("08000",unlist(cty_str))
  cty_name <- CountyName(cty_str)
  cty_num <- as.numeric(str_sub(cty_str,3,5))
  name1 <- listID$plName1
  
# Building the data files  
# Population Estimates age LT05, 0517, 1824, 2544,4564, GE65

  f.USage_grp <- inData[["US"]] %>%  filter(AGE != 999) %>% 
    filter(SEX == 0) %>%
    mutate(age_group = ifelse(AGE <= 4,"AGELT05_EST",
                       ifelse(AGE <= 17, "AGE0517_EST",
                       ifelse(AGE <= 24, "AGE1824_EST",
                       ifelse(AGE <= 44, "AGE2544_EST",
                       ifelse(AGE <= 64, "AGE4564_EST", "AGEGE65_EST")))))) %>%
    group_by(GEOID, age_group) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    mutate(countyname = "United States") %>%
      spread(age_group, totalpopulation) %>%
      mutate(TOTAL_EST = AGELT05_EST + AGE0517_EST + AGE1824_EST + AGE2544_EST + AGE4564_EST + AGEGE65_EST ) %>%
    select(GEOID, GEOID, countyname,
           AGELT05_EST, AGE0517_EST, AGE1824_EST, AGE2544_EST, AGE4564_EST, AGEGE65_EST, TOTAL_EST) 
    

  f.COAge_long <- inData[["CO"]] %>% filter(GEOID %in% cty_str2) %>%
    mutate(age_group = ifelse(age <= 4,"AGELT05_EST",
                       ifelse(age <= 17, "AGE0517_EST",
                       ifelse(age <= 24, "AGE1824_EST",
                       ifelse(age <= 44, "AGE2544_EST",
                       ifelse(age <= 64, "AGE4564_EST", "AGEGE65_EST"))))))   %>%
    group_by(GEOID, countyname, age_group) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    spread(age_group, totalpopulation) %>%
    mutate(TOTAL_EST = AGELT05_EST + AGE0517_EST + AGE1824_EST + AGE2544_EST + AGE4564_EST + AGEGE65_EST ) %>%
    select(GEOID, GEOID, countyname,
           AGELT05_EST, AGE0517_EST, AGE1824_EST, AGE2544_EST, AGE4564_EST, AGEGE65_EST, TOTAL_EST) 
  

  if(length(cty_str) > 1){  #Summing up CSBG Entity
    f.csbg_age <- f.COAge_long %>% filter(GEOID %in% cty_str) %>%
      mutate(year = 1) %>%
      group_by(year) %>%
      summarize(AGELT05_EST = sum(AGELT05_EST),
                AGE0517_EST = sum(AGE0517_EST),
                AGE1824_EST = sum(AGE1824_EST),
                AGE2544_EST = sum(AGE2544_EST),                
                AGE4564_EST = sum(AGE4564_EST),                
                AGEGE65_EST = sum(AGEGE65_EST),                
                TOTAL_EST = sum(TOTAL_EST))  %>%
      mutate(GEOID = 0.500,
             countyname = listID$plName1,
             GEOID = "08000.5") %>%
      select(GEOID, GEOID, countyname,
             AGELT05_EST, AGE0517_EST, AGE1824_EST, AGE2544_EST, AGE4564_EST, AGEGE65_EST, TOTAL_EST) 
    
    f.state_age <- f.COAge_long %>% filter(GEOID == "08000")
    f.cty_age  <- f.COAge_long %>% filter(GEOID %in% cty_str) 
    f.age_sya <- bind_rows(f.USage_grp, f.state_age, f.csbg_age, f.cty_age)
  } else {
    f.age_sya <- bind_rows(f.USage_grp, f.COAge_long) 
  }
  

# Extracting Poverty table

  # POVerty US
  f.B17001_ACS <- inData[["B17001"]]  %>%
     mutate(
      TOT_TOT_EST = .[[3]],	
      TOT_TOT_MOE = .[[4]]^2,
      POV_TOT_EST = .[[5]],	
      POV_TOT_MOE = .[[6]]^2,
      POV_LT05_EST = .[[9]] + .[[37]],	
      POV_LT05_MOE = .[[10]]^2 + .[[38]]^2,
      POV_0517_EST = .[[11]] + .[[13]] + .[[15]] +  .[[17]] + .[[19]] + .[[39]] + .[[41]] + .[[43]] + .[[45]] + .[[47]],	
      POV_0517_MOE = .[[12]]^2 + .[[14]]^2 + .[[16]]^2 +  .[[18]]^2 + .[[20]]^2 + .[[40]]^2 + .[[42]]^2 + .[[44]]^2 + .[[46]]^2 + .[[48]]^2,
      POV_1824_EST = .[[21]] + .[[49]],	
      POV_1824_MOE = .[[22]]^2 + .[[50]]^2,
      POV_2544_EST = .[[23]] + .[[25]] + .[[51]] + .[[53]],	
      POV_2544_MOE = .[[24]]^2 + .[[26]]^2 + .[[52]]^2 + .[[54]]^2,
      POV_4564_EST = .[[27]] + .[[29]] + .[[55]] + .[[57]],	
      POV_4564_MOE = .[[28]]^2 + .[[30]]^2 + .[[56]]^2 + .[[58]]^2,
      POV_GE65_EST = .[[31]] + .[[33]] + .[[59]] + .[[61]],	
      POV_GE65_MOE = .[[32]]^2 + .[[34]]^2 + .[[60]]^2 + .[[62]]^2,
      TOT_LT05_EST =  .[[9]] + .[[37]] +  .[[67]] + .[[95]],
      TOT_LT05_MOE =  .[[10]]^2 + .[[38]]^2 +  .[[68]]^2 + .[[96]]^2,
      TOT_0517_EST =  .[[11]] + .[[13]] + .[[15]] +  .[[17]] + .[[19]] + .[[39]] + .[[41]] + .[[43]] + .[[45]] + .[[47]] +  .[[69]] + .[[71]] + .[[73]] +  .[[75]] + .[[77]] + .[[97]] + .[[99]] + .[[101]] + .[[103]] + .[[105]],
      TOT_0517_MOE =  .[[12]]^2 + .[[14]]^2 + .[[16]]^2 +  .[[18]]^2 + .[[20]]^2 + .[[40]]^2 + .[[42]]^2 + .[[44]]^2 + .[[46]]^2 + .[[48]]^2 +  .[[70]]^2 + .[[72]]^2 + .[[74]]^2 +  .[[76]]^2 + .[[78]]^2 + .[[98]]^2 + .[[100]]^2 + .[[102]]^2 + .[[104]]^2 + .[[106]]^2,
      TOT_1824_EST =  .[[21]] + .[[49]] +  .[[79]] + .[[107]],
      TOT_1824_MOE =  .[[22]]^2 + .[[50]]^2 +  .[[80]]^2 + .[[108]]^2,
      TOT_2544_EST =  .[[23]] + .[[25]] + .[[51]] + .[[53]] +  .[[81]] + .[[83]] + .[[109]] + .[[111]],
      TOT_2544_MOE =  .[[24]]^2 + .[[26]]^2 + .[[52]]^2 + .[[54]]^2 +  .[[82]]^2 + .[[84]]^2 + .[[110]]^2 + .[[112]]^2,
      TOT_4564_EST =  .[[27]] + .[[29]] + .[[55]] + .[[57]] +  .[[85]] + .[[87]] + .[[113]] + .[[115]],
      TOT_4564_MOE =  .[[28]]^2 + .[[30]]^2 + .[[56]]^2 + .[[58]]^2 +  .[[86]]^2 + .[[88]]^2 + .[[114]]^2 + .[[116]]^2,
      TOT_GE65_EST =  .[[31]] + .[[33]] + .[[59]] + .[[61]] +  .[[89]] + .[[91]] + .[[117]] + .[[119]],
      TOT_GE65_MOE =  .[[32]]^2 + .[[34]]^2 + .[[60]]^2 + .[[62]]^2 +  .[[90]]^2 + .[[92]]^2 + .[[118]]^2 + .[[120]]^2
    )  %>%
    select(GEOID, NAME, TOT_TOT_EST : TOT_GE65_MOE)
 
  if(length(cty_str) > 1) {  #Summarize for Multi County Place
    f.B17001_csbg <- f.B17001_ACS %>% filter(GEOID %in% cty_str) %>%
      summarise(TOT_TOT_EST = sum(TOT_TOT_EST, na.rm=TRUE),
                TOT_TOT_MOE = sum(TOT_TOT_MOE, na.rm=TRUE),
                POV_TOT_EST = sum(POV_TOT_EST, na.rm=TRUE),
                POV_TOT_MOE = sum(POV_TOT_MOE, na.rm=TRUE),
                POV_LT05_EST = sum(POV_LT05_EST, na.rm=TRUE),
                POV_LT05_MOE = sum(POV_LT05_MOE, na.rm=TRUE),
                POV_0517_EST = sum(POV_0517_EST, na.rm=TRUE),
                POV_0517_MOE = sum(POV_0517_MOE, na.rm=TRUE),
                POV_1824_EST = sum(POV_1824_EST, na.rm=TRUE),
                POV_1824_MOE = sum(POV_1824_MOE, na.rm=TRUE),
                POV_2544_EST = sum(POV_2544_EST, na.rm=TRUE),
                POV_2544_MOE = sum(POV_2544_MOE, na.rm=TRUE),
                POV_4564_EST = sum(POV_4564_EST, na.rm=TRUE),
                POV_4564_MOE = sum(POV_4564_MOE, na.rm=TRUE),
                POV_GE65_EST = sum(POV_GE65_EST, na.rm=TRUE),
                POV_GE65_MOE = sum(POV_GE65_MOE, na.rm=TRUE),
                TOT_LT05_EST = sum(TOT_LT05_EST, na.rm=TRUE),
                TOT_LT05_MOE = sum(TOT_LT05_MOE, na.rm=TRUE),
                TOT_0517_EST = sum(TOT_0517_EST, na.rm=TRUE),
                TOT_0517_MOE = sum(TOT_0517_MOE, na.rm=TRUE),
                TOT_1824_EST = sum(TOT_1824_EST, na.rm=TRUE),
                TOT_1824_MOE = sum(TOT_1824_MOE, na.rm=TRUE),
                TOT_2544_EST = sum(TOT_2544_EST, na.rm=TRUE),
                TOT_2544_MOE = sum(TOT_2544_MOE, na.rm=TRUE),
                TOT_4564_EST = sum(TOT_4564_EST, na.rm=TRUE),
                TOT_4564_MOE = sum(TOT_4564_MOE, na.rm=TRUE),
                TOT_GE65_EST = sum(TOT_GE65_EST, na.rm=TRUE),
                TOT_GE65_MOE = sum(TOT_GE65_MOE, na.rm=TRUE)) %>%
      mutate(GEOID = "08000.5",
             NAME = name1) %>%
      select(GEOID, NAME, TOT_TOT_EST : TOT_GE65_MOE)
    
    f.B17001_US <- f.B17001_ACS %>% filter(GEOID == "01000")
    f.B17001_CO <- f.B17001_ACS %>% filter(GEOID == "08000")
    f.B17001_CTY <- f.B17001_ACS %>% filter(GEOID %in% cty_str)
    
    f.B17001_FIN <- bind_rows(f.B17001_US, f.B17001_CO, f.B17001_csbg, f.B17001_CTY)
  } else {
    f.B17001_FIN <- f.B17001_ACS %>% filter(GEOID %in% full_str)
  }
  
  
  # calculating Percentage
  f.B17001_FIN <- f.B17001_FIN %>%
    mutate(NAME = str_replace(NAME, ", Colorado",""),
           TOT_TOT_MOE = sqrt(TOT_TOT_MOE),
           POV_TOT_MOE = sqrt(POV_TOT_MOE),
           POV_LT05_MOE = sqrt(POV_LT05_MOE),
           POV_0517_MOE = sqrt(POV_0517_MOE),
           POV_1824_MOE = sqrt(POV_1824_MOE),
           POV_2544_MOE = sqrt(POV_2544_MOE),
           POV_4564_MOE = sqrt(POV_4564_MOE),
           POV_GE65_MOE = sqrt(POV_GE65_MOE),
           TOT_LT05_MOE = sqrt(TOT_LT05_MOE),
           TOT_0517_MOE = sqrt(TOT_0517_MOE),
           TOT_1824_MOE = sqrt(TOT_1824_MOE),
           TOT_2544_MOE = sqrt(TOT_2544_MOE),
           TOT_4564_MOE = sqrt(TOT_4564_MOE),
           TOT_GE65_MOE = sqrt(TOT_GE65_MOE),
           POV_TOT_EST_PCT = POV_TOT_EST/TOT_TOT_EST,
           POV_TOT_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, POV_TOT_MOE,POV_TOT_EST_PCT),
           POV_LT05_EST_PCT = POV_LT05_EST/POV_TOT_EST,
           POV_LT05_MOE_PCT = pctMOE(POV_TOT_EST, POV_TOT_MOE, POV_LT05_MOE,POV_LT05_EST_PCT),
           POV_0517_EST_PCT = POV_0517_EST/POV_TOT_EST,
           POV_0517_MOE_PCT = pctMOE(POV_TOT_EST, POV_TOT_MOE, POV_0517_MOE,POV_0517_EST_PCT),
           POV_1824_EST_PCT = POV_1824_EST/POV_TOT_EST,
           POV_1824_MOE_PCT = pctMOE(POV_TOT_EST, POV_TOT_MOE, POV_1824_MOE,POV_1824_EST_PCT),
           POV_2544_EST_PCT = POV_2544_EST/POV_TOT_EST,
           POV_2544_MOE_PCT = pctMOE(POV_TOT_EST, POV_TOT_MOE, POV_2544_MOE,POV_2544_EST_PCT),
           POV_4564_EST_PCT = POV_4564_EST/POV_TOT_EST,
           POV_4564_MOE_PCT = pctMOE(POV_TOT_EST, POV_TOT_MOE, POV_4564_MOE,POV_4564_EST_PCT),
           POV_GE65_EST_PCT = POV_GE65_EST/POV_TOT_EST,
           POV_GE65_MOE_PCT = pctMOE(POV_TOT_EST, POV_TOT_MOE, POV_GE65_MOE,POV_GE65_EST_PCT),
           TOT_LT05_EST_PCT = TOT_LT05_EST/TOT_TOT_EST,
           TOT_LT05_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, TOT_LT05_MOE,TOT_LT05_EST_PCT),
           TOT_0517_EST_PCT = TOT_0517_EST/TOT_TOT_EST,
           TOT_0517_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, TOT_0517_MOE,TOT_0517_EST_PCT),
           TOT_1824_EST_PCT = TOT_1824_EST/TOT_TOT_EST,
           TOT_1824_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, TOT_1824_MOE,TOT_1824_EST_PCT),
           TOT_2544_EST_PCT = TOT_2544_EST/TOT_TOT_EST,
           TOT_2544_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, TOT_2544_MOE,TOT_2544_EST_PCT),
           TOT_4564_EST_PCT = TOT_4564_EST/TOT_TOT_EST,
           TOT_4564_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, TOT_4564_MOE,TOT_4564_EST_PCT),
           TOT_GE65_EST_PCT = TOT_GE65_EST/TOT_TOT_EST,
           TOT_GE65_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, TOT_GE65_MOE,TOT_GE65_EST_PCT))  %>% 
    arrange(GEOID) %>%
    select(GEOID, NAME, TOT_TOT_EST : TOT_GE65_MOE_PCT)
  
  
  # Building Chart data sets
  f.povage_est <- f.B17001_FIN %>% 
    select(GEOID, NAME, POV_TOT_EST_PCT,	POV_LT05_EST_PCT,	POV_0517_EST_PCT,	POV_1824_EST_PCT,	POV_2544_EST_PCT,	
           POV_4564_EST_PCT,	POV_GE65_EST_PCT,	TOT_LT05_EST_PCT,	TOT_0517_EST_PCT,	TOT_1824_EST_PCT,	TOT_2544_EST_PCT,	
           TOT_4564_EST_PCT,	TOT_GE65_EST_PCT) %>%
    gather(POV_AGE, EST_PCT,POV_TOT_EST_PCT : TOT_GE65_EST_PCT, factor_key =TRUE) %>%
    mutate(POV_AGE = str_replace(POV_AGE,"_EST_PCT",""))
  
  f.povage_moe <- f.B17001_FIN %>% 
    select(GEOID, POV_TOT_MOE_PCT,	POV_LT05_MOE_PCT,	POV_0517_MOE_PCT,	POV_1824_MOE_PCT,	POV_2544_MOE_PCT,	
           POV_4564_MOE_PCT,	POV_GE65_MOE_PCT,	TOT_LT05_MOE_PCT,	TOT_0517_MOE_PCT,	TOT_1824_MOE_PCT,	TOT_2544_MOE_PCT,	
           TOT_4564_MOE_PCT,	TOT_GE65_MOE_PCT) %>%
    gather(POV_AGE, MOE_PCT,POV_TOT_MOE_PCT : TOT_GE65_MOE_PCT, factor_key =TRUE) %>%
    mutate(POV_AGE = str_replace(POV_AGE,"_MOE_PCT",""))
  
  f.povage_chart <- inner_join(f.povage_est, f.povage_moe, by =c('GEOID',"POV_AGE")) %>%
    mutate(EST_PCT = EST_PCT * 100,
           MOE_PCT = MOE_PCT * 100,
           TYPE = ifelse(grepl("POV",POV_AGE),0,1),
           POV_AGE = str_replace(POV_AGE,"POV_",""),
           POV_AGE = str_replace(POV_AGE,"TOT_","")) %>%
   filter(POV_AGE != "TOT")
  f.povage_chart_tab <- inner_join(f.povage_est, f.povage_moe, by =c('GEOID',"POV_AGE"))


# Generating Charts...
f.povage_chart$POV_AGE <- factor(f.povage_chart$POV_AGE, levels = c("LT05", "0517", "1824","2544", "4564", "GE65"),
                      labels = c("0 to 4", "5 to 17", "18 to 24", "25 to 44",
                                 "45 to 64", "65 and Older"))
                      
f.povage_chart$TYPE <- factor(f.povage_chart$TYPE, levels=c(0,1),
                              labels =c("Below Federal Poverty Level",
                                        "Total Population"))

maxLim <- max(f.povage_chart$EST_PCT) + 10

chart_list <- list()
pltTitle <- paste0("Age by Poverty Status: ", listID$plName1)


if(length(cty_str) > 1) {
  f.povage_chart3 <- f.povage_chart %>% filter(!(GEOID %in% cty_str))
} else {
  f.povage_chart3 <- f.povage_chart
}

nameList <- unique(f.povage_chart3$NAME)
f.povage_chart3$NAME <- factor(f.povage_chart3$NAME, levels =nameList)

# Creating Bar Charts
ggimg3 <-ggplot( f.povage_chart3, aes(x =as.factor(POV_AGE), y =EST_PCT, fill = NAME)) +
  geom_bar(stat ="identity", position = "dodge", color ="black") + 
  geom_text(aes(x =as.factor(POV_AGE), y =EST_PCT, label =percent(EST_PCT), group = NAME), 
            position = position_dodge(width =1.0),
            vjust =-0.8,  size =3) +
  scale_fill_manual(values =barCol) +
  scale_y_continuous(limits = c(0, maxLim), label =percent, expand = c(0, 0)) +
  facet_wrap(vars(TYPE), ncol=1) +
  labs(title = pltTitle,
       subtitle = "CSBG Comparison",
       caption = outCap,
       x = "Age Group",
       y = "Percentage",
       fill ="CSBG Entity") +
  theme(plot.title = element_text(hjust = 0.5, size =12),
        plot.caption = element_text(hjust = 0, size =9),
        panel.background = element_rect(fill = "white", colour = "gray50"),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(size =8),
        axis.text.y =element_text(size =8),
        legend.position = "bottom", 
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font siz

chart_list[[1]] <- ggimg3

if(length(cty_str) > 1) {
for(i in 1:length(cty_str)) {
  n <- i + 1
  f.chart_data <- f.povage_chart %>% filter(GEOID == "08000.5" | GEOID == cty_str[i]) %>% arrange(GEOID)


  ggimg3 <-ggplot( f.chart_data, aes(x =as.factor(POV_AGE), y =EST_PCT, fill = NAME)) +
    geom_bar(stat ="identity", position = "dodge", color ="black") + 
    geom_text(aes(x =as.factor(POV_AGE), y =EST_PCT, label =percent(EST_PCT), group = NAME), 
              position = position_dodge(width =1.0),
              vjust =-0.8,  size =3) +
    scale_fill_manual(values =barCol2) +
    scale_y_continuous(limits = c(0, maxLim), label =percent, expand = c(0, 0)) +
    facet_wrap(vars(TYPE), ncol=1) +
    labs(title = pltTitle,
         subtitle = "County Comparison",
         caption = outCap,
         x = "Age Group",
         y = "Percentage",
         fill ="CSBG Entity") +
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


    
# Creating Table data file
# Modifying Age table
f.pov_age_tab_adj  <- f.B17001_FIN %>% 
       select(GEOID, TOT_TOT_MOE, POV_TOT_EST_PCT,	POV_TOT_MOE_PCT,	POV_LT05_EST_PCT,	POV_LT05_MOE_PCT,	
              POV_0517_EST_PCT,	POV_0517_MOE_PCT,	POV_1824_EST_PCT,	POV_1824_MOE_PCT,	POV_2544_EST_PCT,	
              POV_2544_MOE_PCT,	POV_4564_EST_PCT,	POV_4564_MOE_PCT,	POV_GE65_EST_PCT,	POV_GE65_MOE_PCT,	
              TOT_LT05_EST_PCT,	TOT_LT05_MOE_PCT,	TOT_0517_EST_PCT,	TOT_0517_MOE_PCT,	TOT_1824_EST_PCT,	
              TOT_1824_MOE_PCT,	TOT_2544_EST_PCT,	TOT_2544_MOE_PCT,	TOT_4564_EST_PCT,	TOT_4564_MOE_PCT,	
              TOT_GE65_EST_PCT,	TOT_GE65_MOE_PCT) %>%
              inner_join(., f.age_sya, by="GEOID") %>%
     mutate( 
            TOT_TOT_EST_ADJ = TOTAL_EST,
            TOT_TOT_MOE_ADJ = TOT_TOT_MOE,
            TOT_TOT_EST_PCT = 1,
            TOT_TOT_MOE_PCT = 0,
            POV_TOT_EST_ADJ = POV_TOT_EST_PCT * TOTAL_EST,
            POV_TOT_MOE_ADJ = POV_TOT_MOE_PCT * TOTAL_EST,
            POV_LT05_EST_ADJ = POV_LT05_EST_PCT * POV_TOT_EST_ADJ,
            POV_LT05_MOE_ADJ = POV_LT05_MOE_PCT * POV_TOT_MOE_ADJ,
            POV_0517_EST_ADJ = POV_0517_EST_PCT * POV_TOT_EST_ADJ,
            POV_0517_MOE_ADJ = POV_0517_MOE_PCT * POV_TOT_MOE_ADJ,
            POV_1824_EST_ADJ = POV_1824_EST_PCT * POV_TOT_EST_ADJ,
            POV_1824_MOE_ADJ = POV_1824_MOE_PCT * POV_TOT_MOE_ADJ,
            POV_2544_EST_ADJ = POV_2544_EST_PCT * POV_TOT_EST_ADJ,
            POV_2544_MOE_ADJ = POV_2544_MOE_PCT * POV_TOT_MOE_ADJ,
            POV_4564_EST_ADJ = POV_4564_EST_PCT * POV_TOT_EST_ADJ,
            POV_4564_MOE_ADJ = POV_4564_MOE_PCT * POV_TOT_MOE_ADJ,
            POV_GE65_EST_ADJ = POV_GE65_EST_PCT * POV_TOT_EST_ADJ,
            POV_GE65_MOE_ADJ = POV_GE65_MOE_PCT * POV_TOT_MOE_ADJ,
            TOT_LT05_EST_ADJ = TOT_LT05_EST_PCT * TOTAL_EST,
            TOT_LT05_MOE_ADJ = TOT_LT05_MOE_PCT * TOTAL_EST,
            TOT_0517_EST_ADJ = TOT_0517_EST_PCT * TOTAL_EST,
            TOT_0517_MOE_ADJ = TOT_0517_MOE_PCT * TOTAL_EST,
            TOT_1824_EST_ADJ = TOT_1824_EST_PCT * TOTAL_EST,
            TOT_1824_MOE_ADJ = TOT_1824_MOE_PCT * TOTAL_EST,
            TOT_2544_EST_ADJ = TOT_2544_EST_PCT * TOTAL_EST,
            TOT_2544_MOE_ADJ = TOT_2544_MOE_PCT * TOTAL_EST,
            TOT_4564_EST_ADJ = TOT_4564_EST_PCT * TOTAL_EST,
            TOT_4564_MOE_ADJ = TOT_4564_MOE_PCT * TOTAL_EST,
            TOT_GE65_EST_ADJ = TOT_GE65_EST_PCT * TOTAL_EST,
            TOT_GE65_MOE_ADJ = TOT_GE65_MOE_PCT * TOTAL_EST,
            TOT_TOT_EST_PCT = TOT_TOT_EST_PCT * 100,
            TOT_TOT_MOE_PCT = TOT_TOT_MOE_PCT * 100,
            POV_TOT_EST_PCT  =  POV_TOT_EST_PCT  * 100,
            POV_TOT_MOE_PCT  =  POV_TOT_MOE_PCT  * 100,
            POV_LT05_EST_PCT  =  POV_LT05_EST_PCT  * 100,
            POV_LT05_MOE_PCT  =  POV_LT05_MOE_PCT  * 100,
            POV_0517_EST_PCT  =  POV_0517_EST_PCT  * 100,
            POV_0517_MOE_PCT  =  POV_0517_MOE_PCT  * 100,
            POV_1824_EST_PCT  =  POV_1824_EST_PCT  * 100,
            POV_1824_MOE_PCT  =  POV_1824_MOE_PCT  * 100,
            POV_2544_EST_PCT  =  POV_2544_EST_PCT  * 100,
            POV_2544_MOE_PCT  =  POV_2544_MOE_PCT  * 100,
            POV_4564_EST_PCT  =  POV_4564_EST_PCT  * 100,
            POV_4564_MOE_PCT  =  POV_4564_MOE_PCT  * 100,
            POV_GE65_EST_PCT  =  POV_GE65_EST_PCT  * 100,
            POV_GE65_MOE_PCT  =  POV_GE65_MOE_PCT  * 100,
            TOT_LT05_EST_PCT  =  TOT_LT05_EST_PCT  * 100,
            TOT_LT05_MOE_PCT  =  TOT_LT05_MOE_PCT  * 100,
            TOT_0517_EST_PCT  =  TOT_0517_EST_PCT  * 100,
            TOT_0517_MOE_PCT  =  TOT_0517_MOE_PCT  * 100,
            TOT_1824_EST_PCT  =  TOT_1824_EST_PCT  * 100,
            TOT_1824_MOE_PCT  =  TOT_1824_MOE_PCT  * 100,
            TOT_2544_EST_PCT  =  TOT_2544_EST_PCT  * 100,
            TOT_2544_MOE_PCT  =  TOT_2544_MOE_PCT  * 100,
            TOT_4564_EST_PCT  =  TOT_4564_EST_PCT  * 100,
            TOT_4564_MOE_PCT  =  TOT_4564_MOE_PCT  * 100,
            TOT_GE65_EST_PCT  =  TOT_GE65_EST_PCT  * 100,
            TOT_GE65_MOE_PCT  =  TOT_GE65_MOE_PCT  * 100) 


f.pov_age_tab_pov <- f.pov_age_tab_adj %>%
          mutate(TYPE = "Below Federal Poverty Level") %>%
          select(GEOID, countyname, TYPE, POV_LT05_EST_ADJ,	POV_LT05_MOE_ADJ,	POV_LT05_EST_PCT,	POV_LT05_MOE_PCT,
                 POV_0517_EST_ADJ,	POV_0517_MOE_ADJ,	POV_0517_EST_PCT,	POV_0517_MOE_PCT,
                 POV_1824_EST_ADJ,	POV_1824_MOE_ADJ,	POV_1824_EST_PCT,	POV_1824_MOE_PCT,
                 POV_2544_EST_ADJ,	POV_2544_MOE_ADJ,	POV_2544_EST_PCT,	POV_2544_MOE_PCT,
                 POV_4564_EST_ADJ,	POV_4564_MOE_ADJ,	POV_4564_EST_PCT,	POV_4564_MOE_PCT,
                 POV_GE65_EST_ADJ,	POV_GE65_MOE_ADJ,	POV_GE65_EST_PCT,	POV_GE65_MOE_PCT,
                 POV_TOT_EST_ADJ,	POV_TOT_MOE_ADJ,	POV_TOT_EST_PCT,	POV_TOT_MOE_PCT)

names(f.pov_age_tab_pov) <- c("GEOID", "NAME","TYPE", "V_LT05_EST_ADJ",	"V_LT05_MOE_ADJ",	"V_LT05_EST_PCT",	"V_LT05_MOE_PCT",
                              "V_0517_EST_ADJ",	"V_0517_MOE_ADJ",	"V_0517_EST_PCT",	"V_0517_MOE_PCT",
                              "V_1824_EST_ADJ",	"V_1824_MOE_ADJ",	"V_1824_EST_PCT",	"V_1824_MOE_PCT",
                              "V_2544_EST_ADJ",	"V_2544_MOE_ADJ",	"V_2544_EST_PCT",	"V_2544_MOE_PCT",
                              "V_4564_EST_ADJ",	"V_4564_MOE_ADJ",	"V_4564_EST_PCT",	"V_4564_MOE_PCT",
                              "V_GE65_EST_ADJ",	"V_GE65_MOE_ADJ",	"V_GE65_EST_PCT",	"V_GE65_MOE_PCT",
                              "V_TOT_EST_ADJ",	"V_TOT_MOE_ADJ",	"V_TOT_EST_PCT",	"V_TOT_MOE_PCT") 

f.pov_age_tab_tot <- f.pov_age_tab_adj %>%
  mutate(TYPE = "Total Population" ) %>%
  select(GEOID, countyname, TYPE, TOT_LT05_EST_ADJ,	TOT_LT05_MOE_ADJ,	TOT_LT05_EST_PCT,	TOT_LT05_MOE_PCT,
         TOT_0517_EST_ADJ,	TOT_0517_MOE_ADJ,	TOT_0517_EST_PCT,	TOT_0517_MOE_PCT,
         TOT_1824_EST_ADJ,	TOT_1824_MOE_ADJ,	TOT_1824_EST_PCT,	TOT_1824_MOE_PCT,
         TOT_2544_EST_ADJ,	TOT_2544_MOE_ADJ,	TOT_2544_EST_PCT,	TOT_2544_MOE_PCT,
         TOT_4564_EST_ADJ,	TOT_4564_MOE_ADJ,	TOT_4564_EST_PCT,	TOT_4564_MOE_PCT,
         TOT_GE65_EST_ADJ,	TOT_GE65_MOE_ADJ,	TOT_GE65_EST_PCT,	TOT_GE65_MOE_PCT,
         TOT_TOT_EST_ADJ, TOT_TOT_MOE_ADJ, TOT_TOT_EST_PCT, TOT_TOT_MOE_PCT)

names(f.pov_age_tab_tot) <- c("GEOID", "NAME","TYPE", "V_LT05_EST_ADJ",	"V_LT05_MOE_ADJ",	"V_LT05_EST_PCT",	"V_LT05_MOE_PCT",
                              "V_0517_EST_ADJ",	"V_0517_MOE_ADJ",	"V_0517_EST_PCT",	"V_0517_MOE_PCT",
                              "V_1824_EST_ADJ",	"V_1824_MOE_ADJ",	"V_1824_EST_PCT",	"V_1824_MOE_PCT",
                              "V_2544_EST_ADJ",	"V_2544_MOE_ADJ",	"V_2544_EST_PCT",	"V_2544_MOE_PCT",
                              "V_4564_EST_ADJ",	"V_4564_MOE_ADJ",	"V_4564_EST_PCT",	"V_4564_MOE_PCT",
                              "V_GE65_EST_ADJ",	"V_GE65_MOE_ADJ",	"V_GE65_EST_PCT",	"V_GE65_MOE_PCT",
                              "V_TOT_EST_ADJ",	"V_TOT_MOE_ADJ",	"V_TOT_EST_PCT",	"V_TOT_MOE_PCT") 

f.pop_age_tab_fin <- bind_rows(f.pov_age_tab_pov, f.pov_age_tab_tot)   %>% arrange(GEOID, TYPE) %>% 
                  mutate(NAME = ifelse(TYPE == "Total Population","",NAME)) %>%
                   select(-GEOID)


# formatting
f.pop_age_tab_fin[,c(3,4,7,8,11,12,15,16,19,20,23,24,27,28)] <- sapply(f.pop_age_tab_fin[,c(3,4,7,8,11,12,15,16,19,20,23,24,27,28)],function(x) comma_conv(x))
f.pop_age_tab_fin[,c(5,6,9,10,13,14,17,18,21,22,25,26,29,30)] <- sapply(f.pop_age_tab_fin[,c(5,6,9,10,13,14,17,18,21,22,25,26,29,30)],function(x) percent(x))

# Flex Table
  tab_head <- paste0("Age by Percent Below Federal Poverty Status, ",listID$plName1, " ",curYr)
  head_level <- c("","","0 to 4","","","",
                  "5 to 17","","","",
                  "18 to 24","","","",
                  "25 to 44","","","",
                  "45 to 64","","","",
                  "65 and Older","","","",
                  "Total","","","")
  head_measure <- c("CSBG Entity","Poverty Status",rep(c("Estimate","Margin of Error"),14))
  
  headers <-t(bind_cols(as.data.frame(head_level), as.data.frame(head_measure)))
 
  f.povFlex <- flextable(
    f.pop_age_tab_fin,
    col_keys = names(f.pop_age_tab_fin)) %>%
    add_header_row(values =head_measure,top =TRUE) %>%
    add_header_row(values =head_level,top =TRUE) %>%
    align(j =1:30,align ="center",part ="header") %>%
    delete_rows(i = 3, part = "header") %>%
    merge_at(i =1,j =3:6, part ="header") %>%
    merge_at(i =1,j =7:10, part ="header") %>%
    merge_at(i =1,j =11:14, part ="header") %>%
    merge_at(i =1,j =15:18, part ="header") %>%
    merge_at(i =1,j =19:22, part ="header") %>%
    merge_at(i =1,j =23:26, part ="header") %>%
    merge_at(i =1,j =27:30, part ="header") %>%
    align(j =1:2, align ="left", part ="body") %>%
    align(j =3:30, align ="right", part ="body") %>%
    width(j =1:2, width =0.7) %>%
    width(j =3:30,width =0.7) %>%
    fontsize(size = 7, part = "all")
  
  f.povFlex <- add_header_lines(f.povFlex,values =tab_head, top =TRUE)

  #bind list
  outList <- list("plot"= chart_list, "data" = f.pop_age_tab_fin, "header_sh" = headers, "FlexTable" = f.povFlex)
  
  return(outList)
}
