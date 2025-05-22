#'disabilityPRO Outputs Tables and plots for the Disability by Age and FPL
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
disabilityPRO <- function(listID,curYr, inData, barCol, barCol2) {
  # Collecting place ids from  idList, setting default values
  # Collecting List of Counties
  outCap <- captionSrc("ACS",curYr,"C18130") 
  cty_str <- listID$list2
  full_str <- c("01000","08000",unlist(cty_str))
  cty_str2 <- c("08000",unlist(cty_str))
  cty_name <- CountyName(cty_str)
  cty_num <- as.numeric(str_sub(cty_str,3,5))
  name1 <- listID$plName1
  
  
  # Collecting US Data 

  f.USage_grp <- inData[["US"]] %>%  filter(AGE != 999) %>% 
    filter(SEX == 0) %>%
    mutate(age_group = ifelse(AGE <= 17,"AGELT18_EST",
                              ifelse(AGE <= 64, "AGE1864_EST","AGEGE65_EST"))
    ) %>%
    group_by(GEOID, age_group) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    mutate( countyname = "United States") %>%
    spread(age_group, totalpopulation) %>%
    mutate(TOTAL_EST = AGELT18_EST + AGE1864_EST + AGEGE65_EST ) %>%
    select(GEOID, countyname,
           AGELT18_EST, AGE1864_EST, AGEGE65_EST, TOTAL_EST) 
  
  
  
  
  f.COAge_long <- inData[["CO"]] %>% filter(GEOID %in% cty_str2) %>%
    mutate(age_group = ifelse(age <= 17,"AGELT18_EST",
                              ifelse(age <= 64, "AGE1864_EST", "AGEGE65_EST")))   %>%
    group_by(GEOID, countyname, age_group) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    ungroup() %>%
    spread(age_group, totalpopulation) %>%
    mutate(TOTAL_EST = AGELT18_EST + AGE1864_EST + AGEGE65_EST ) %>%
    select(GEOID, countyname,
           AGELT18_EST, AGE1864_EST, AGEGE65_EST, TOTAL_EST) 
  
  
  if(length(listID$list2) > 1){  #Summing up CSBG Entity
    f.csbg_age <- f.COAge_long %>% filter(GEOID %in% cty_str) %>%
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
    
    f.state_age <- f.COAge_long %>% filter(GEOID == "08000")
    f.cty_age  <- f.COAge_long %>% filter(GEOID %in% cty_str) 
    f.age_sya <- bind_rows(f.USage_grp, f.state_age, f.csbg_age, f.cty_age) 
  } else {
    f.age_sya <- bind_rows(f.USage_grp, f.COAge_long) 
  }
  
  
  
  # POVerty US
  f.C18130_ACS <- inData[["C18130"]] %>% filter(GEOID %in% full_str) %>%
    mutate(
      TOT_TOT_TOT_EST  = .[[3]],	
      TOT_TOT_TOT_MOE  = .[[4]]^2,
      TOT_TOT_LE18_EST  = .[[5]],	
      TOT_TOT_LE18_MOE  = .[[6]]^2,
      TOT_DIS_LE18_EST  = .[[7]],	
      TOT_DIS_LE18_MOE  = .[[8]]^2,
      POV_DIS_LE18_EST  = .[[9]],	
      POV_DIS_LE18_MOE  = .[[10]]^2,
      TOT_TOT_1864_EST  = .[[19]],	
      TOT_TOT_1864_MOE  = .[[20]]^2,
      TOT_DIS_1864_EST  = .[[21]],	
      TOT_DIS_1864_MOE  = .[[22]]^2,
      POV_DIS_1864_EST  = .[[23]],	
      POV_DIS_1864_MOE  = .[[24]]^2,
      TOT_TOT_GE65_EST  = .[[33]],	
      TOT_TOT_GE65_MOE  = .[[34]]^2,
      TOT_DIS_GE65_EST  = .[[35]],	
      TOT_DIS_GE65_MOE  = .[[36]]^2,
      POV_DIS_GE65_EST  = .[[37]],	
      POV_DIS_GE65_MOE  = .[[38]]^2,
      
      TOT_DIS_TOT_EST  = .[[7]] + .[[21]] + .[[35]],	
      TOT_DIS_TOT_MOE  = .[[8]]^2 + .[[22]]^2 + .[[36]]^2,
      POV_DIS_TOT_EST  = .[[9]] + .[[23]] + .[[37]],	
      POV_DIS_TOT_MOE  = .[[10]]^2 + .[[24]]^2 + .[[38]]^2,
      POV_TOT_TOT_EST  = .[[9]] + .[[15]] + .[[23]] + .[[29]] + .[[37]] + .[[43]],	
      POV_TOT_TOT_MOE  = .[[10]]^2 + .[[16]]^2 + .[[24]]^2 + .[[30]]^2 + .[[38]]^2 + .[[44]]^2)  %>%
    select(GEOID, NAME, TOT_TOT_TOT_EST : POV_TOT_TOT_MOE)
  
 
  if(length(cty_str) > 1) {  #Summarize for Multi County Place
    f.C18130_csbg <- f.C18130_ACS %>% filter(GEOID %in% cty_str) %>%
      summarise(TOT_TOT_TOT_EST = sum(TOT_TOT_TOT_EST, na.rm=TRUE),
                TOT_TOT_TOT_MOE = sum(TOT_TOT_TOT_MOE, na.rm=TRUE),
                TOT_TOT_LE18_EST = sum(TOT_TOT_LE18_EST, na.rm=TRUE),
                TOT_TOT_LE18_MOE = sum(TOT_TOT_LE18_MOE, na.rm=TRUE),
                TOT_DIS_LE18_EST = sum(TOT_DIS_LE18_EST, na.rm=TRUE),
                TOT_DIS_LE18_MOE = sum(TOT_DIS_LE18_MOE, na.rm=TRUE),
                POV_DIS_LE18_EST = sum(POV_DIS_LE18_EST, na.rm=TRUE),
                POV_DIS_LE18_MOE = sum(POV_DIS_LE18_MOE, na.rm=TRUE),
                TOT_TOT_1864_EST = sum(TOT_TOT_1864_EST, na.rm=TRUE),
                TOT_TOT_1864_MOE = sum(TOT_TOT_1864_MOE, na.rm=TRUE),
                TOT_DIS_1864_EST = sum(TOT_DIS_1864_EST, na.rm=TRUE),
                TOT_DIS_1864_MOE = sum(TOT_DIS_1864_MOE, na.rm=TRUE),
                POV_DIS_1864_EST = sum(POV_DIS_1864_EST, na.rm=TRUE),
                POV_DIS_1864_MOE = sum(POV_DIS_1864_MOE, na.rm=TRUE),
                TOT_TOT_GE65_EST = sum(TOT_TOT_GE65_EST, na.rm=TRUE),
                TOT_TOT_GE65_MOE = sum(TOT_TOT_GE65_MOE, na.rm=TRUE),
                TOT_DIS_GE65_EST = sum(TOT_DIS_GE65_EST, na.rm=TRUE),
                TOT_DIS_GE65_MOE = sum(TOT_DIS_GE65_MOE, na.rm=TRUE),
                POV_DIS_GE65_EST = sum(POV_DIS_GE65_EST, na.rm=TRUE),
                POV_DIS_GE65_MOE = sum(POV_DIS_GE65_MOE, na.rm=TRUE),
                TOT_DIS_TOT_EST = sum(TOT_DIS_TOT_EST, na.rm=TRUE),
                TOT_DIS_TOT_MOE = sum(TOT_DIS_TOT_MOE, na.rm=TRUE),
                POV_DIS_TOT_EST = sum(POV_DIS_TOT_EST, na.rm=TRUE),
                POV_DIS_TOT_MOE = sum(POV_DIS_TOT_MOE, na.rm=TRUE),
                POV_TOT_TOT_EST = sum(POV_TOT_TOT_EST, na.rm=TRUE),
                POV_TOT_TOT_MOE = sum(POV_TOT_TOT_MOE, na.rm=TRUE)) %>%
      mutate(GEOID = "08000.5",
             NAME = name1) %>%
      select(GEOID, NAME, TOT_TOT_TOT_EST : POV_TOT_TOT_MOE)

    f.C18130_US <- f.C18130_ACS %>% filter(GEOID == "01000")
    f.C18130_CO <- f.C18130_ACS %>% filter(GEOID == "08000")
    f.C18130_CTY <- f.C18130_ACS %>% filter(GEOID %in% cty_str)
    
    f.C18130_FIN <- bind_rows(f.C18130_US, f.C18130_CO, f.C18130_csbg, f.C18130_CTY)
  } else {
    f.C18130_FIN <- f.C18130_ACS %>% filter(GEOID %in% full_str)
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
           TOT_DIS_LE18_MOE_PCT = pctMOE(POV_DIS_TOT_EST, POV_DIS_TOT_MOE,TOT_DIS_LE18_MOE, TOT_DIS_LE18_EST_PCT),
           TOT_DIS_1864_EST_PCT = TOT_DIS_1864_EST/TOT_DIS_TOT_EST,
           TOT_DIS_1864_MOE_PCT = pctMOE(POV_DIS_TOT_EST, POV_DIS_TOT_MOE,TOT_DIS_1864_MOE, TOT_DIS_1864_EST_PCT),
           TOT_DIS_GE65_EST_PCT = TOT_DIS_GE65_EST/TOT_DIS_TOT_EST,
           TOT_DIS_GE65_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE, TOT_DIS_GE65_MOE, TOT_DIS_GE65_EST_PCT),
           TOT_DIS_TOT_EST_PCT = TOT_DIS_TOT_EST/TOT_TOT_TOT_EST,
           TOT_DIS_TOT_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE, TOT_DIS_TOT_MOE, TOT_DIS_TOT_EST_PCT),
           
           POV_DIS_LE18_EST_PCT = POV_DIS_LE18_EST/POV_DIS_TOT_EST,
           POV_DIS_LE18_MOE_PCT = pctMOE(TOT_DIS_TOT_EST, TOT_DIS_TOT_MOE,POV_DIS_LE18_MOE, POV_DIS_LE18_EST_PCT),
           POV_DIS_1864_EST_PCT = POV_DIS_1864_EST/POV_DIS_TOT_EST,
           POV_DIS_1864_MOE_PCT = pctMOE(TOT_DIS_TOT_EST, TOT_DIS_TOT_MOE,POV_DIS_1864_MOE, POV_DIS_1864_EST_PCT),
           POV_DIS_GE65_EST_PCT = POV_DIS_GE65_EST/POV_DIS_TOT_EST,
           POV_DIS_GE65_MOE_PCT = pctMOE(POV_DIS_TOT_EST, POV_DIS_TOT_MOE,TOT_DIS_GE65_MOE, TOT_DIS_GE65_EST_PCT),
           POV_DIS_TOT_EST_PCT = POV_DIS_TOT_EST/POV_TOT_TOT_EST,
           POV_DIS_TOT_MOE_PCT = pctMOE(POV_TOT_TOT_EST,POV_TOT_TOT_MOE, POV_DIS_TOT_MOE, POV_DIS_TOT_EST_PCT)
              )  %>% 
    arrange(GEOID) %>%
    select(GEOID,	NAME,		
           POV_DIS_LE18_EST,	POV_DIS_LE18_MOE,	POV_DIS_LE18_EST_PCT,	POV_DIS_LE18_MOE_PCT,
           POV_DIS_1864_EST,	POV_DIS_1864_MOE,	POV_DIS_1864_EST_PCT,	POV_DIS_1864_MOE_PCT,
           POV_DIS_GE65_EST,	POV_DIS_GE65_MOE,	POV_DIS_GE65_EST_PCT,	POV_DIS_GE65_MOE_PCT,
           POV_DIS_TOT_EST,	  POV_DIS_TOT_MOE,	POV_DIS_TOT_EST_PCT,	POV_DIS_TOT_MOE_PCT,
           TOT_DIS_LE18_EST,	TOT_DIS_LE18_MOE,	TOT_DIS_LE18_EST_PCT,	TOT_DIS_LE18_MOE_PCT,
           TOT_DIS_1864_EST,	TOT_DIS_1864_MOE,	TOT_DIS_1864_EST_PCT,	TOT_DIS_1864_MOE_PCT,
           TOT_DIS_GE65_EST,	TOT_DIS_GE65_MOE,	TOT_DIS_GE65_EST_PCT,	TOT_DIS_GE65_MOE_PCT,
           TOT_DIS_TOT_EST,	  TOT_DIS_TOT_MOE,	TOT_DIS_TOT_EST_PCT,	TOT_DIS_TOT_MOE_PCT)


  # Building Chart data sets
  f.dis_est <- f.C18130_FIN %>% select(GEOID, NAME, POV_DIS_LE18_EST_PCT,	POV_DIS_1864_EST_PCT,	POV_DIS_GE65_EST_PCT,	POV_DIS_TOT_EST_PCT,	
                                       TOT_DIS_LE18_EST_PCT,	TOT_DIS_1864_EST_PCT,	TOT_DIS_GE65_EST_PCT,	TOT_DIS_TOT_EST_PCT) %>%
    gather(dis_AGE, EST_PCT,POV_DIS_LE18_EST_PCT : TOT_DIS_TOT_EST_PCT, factor_key =TRUE) %>%
    mutate(dis_AGE = str_replace(dis_AGE,"_EST_PCT",""))
  
  f.dis_moe <- f.C18130_FIN %>% select(GEOID, NAME, POV_DIS_LE18_MOE_PCT,	POV_DIS_1864_MOE_PCT,	POV_DIS_GE65_MOE_PCT,	POV_DIS_TOT_MOE_PCT,	
                                        TOT_DIS_LE18_MOE_PCT,	TOT_DIS_1864_MOE_PCT,	TOT_DIS_GE65_MOE_PCT,	TOT_DIS_TOT_MOE_PCT) %>%
    gather(dis_AGE, MOE_PCT,POV_DIS_LE18_MOE_PCT : TOT_DIS_TOT_MOE_PCT, factor_key =TRUE) %>%
    mutate(dis_AGE = str_replace(dis_AGE,"_MOE_PCT","")) %>% select(-NAME)
  

  f.dis_chart <- inner_join(f.dis_est, f.dis_moe, by =c("GEOID","dis_AGE")) %>% replace(is.na(.), 0) %>%
    mutate(EST_PCT = EST_PCT * 100,
           MOE_PCT = MOE_PCT * 100,
           TYPE = ifelse(str_sub(dis_AGE,1,3) == "POV",0,1),
           dis_AGE = str_replace(dis_AGE,"POV_DIS_",""),
           dis_AGE = str_replace(dis_AGE,"TOT_DIS_",""))
  f.dis_chart_tab <- f.dis_chart

  f.dis_chart$dis_AGE <- factor(f.dis_chart$dis_AGE, 
                                levels = c("LE18",  "1864", "GE65", "TOT"),
                                labels = c("Age Less Than 18",
                                           "Age 18 to 64",
                                           "Age 65 and older",
                                           "All Persons with Disabilities"))
  
  f.dis_chart$TYPE <- factor(f.dis_chart$TYPE, levels=c(0,1),
                             labels =c("Below Federal Poverty Level",
                                       "Total Population"))
  
  nameList <- unique(f.dis_chart$NAME)
  f.dis_chart$NAME <- factor(f.dis_chart$NAME, levels =nameList)
  
  chart_list <- list()
  pltTitle <- paste0("Persons with Disabilities by Age and Poverty Status: ", name1)
   
  if(length(cty_str) > 1) {
    f.dis_chart3 <- f.dis_chart %>% filter(!(GEOID %in% cty_str))
  } else {
    f.dis_chart3 <- f.dis_chart
  }
  
  maxLim <- max(f.dis_chart$EST_PCT) + 10
  
  # Creating Bar Charts
  ggimg3 <-ggplot( f.dis_chart3, aes(x =as.factor(dis_AGE), y =EST_PCT, fill = NAME)) +
    geom_bar(stat ="identity", position = "dodge", color ="black") + 
    geom_text(aes(x =as.factor(dis_AGE), y =EST_PCT, label =percent(EST_PCT), group = NAME), 
              position = position_dodge(width =1.0),
              vjust =-0.8,  size =4) +
    scale_fill_manual(values =barCol) +
    scale_y_continuous(limits = c(0, maxLim), label =percent, expand = c(0, 0)) +
    facet_wrap(vars(TYPE), ncol=1) +
    labs(title = pltTitle,
         subtitle = "CSBG Comparison",
         caption = outCap,
         x = "Age, Persons with Disabilities",
         y = "Percentage",
         fill ="CSBG Entity",
         alt = pltTitle) +
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
  
  if(length(cty_str) > 1) {
  for(i in 1:length(cty_str)) {
    n <- i + 1
    f.chart_data <- f.dis_chart %>% filter(GEOID == "08000.5" | GEOID == cty_str[i]) 
    
    ggimg3 <-ggplot( f.chart_data, aes(x =as.factor(dis_AGE), y =EST_PCT, fill = NAME)) +
      geom_bar(stat ="identity", position = "dodge", color ="black") + 
      geom_text(aes(x =as.factor(dis_AGE), y =EST_PCT, label =percent(EST_PCT), group = NAME), 
                position = position_dodge(width =1.0),
                vjust =-0.8,  size =4) +
      scale_fill_manual(values =barCol2) +
      scale_y_continuous(limits = c(0, maxLim), label =percent, expand = c(0, 0)) +
      facet_wrap(vars(TYPE), ncol=1) +
      labs(title = pltTitle,
           subtitle = "County Comparison",
           caption = outCap,
           x = "Age, Persons with Disabilities",
           y = "Percentage",
           fill ="CSBG Entity",
           alt = pltTitle) +
      theme(plot.title = element_text(hjust = 0.5, size =12),
            plot.caption = element_text(hjust = 0, size =9),
            panel.background = element_rect(fill = "white", colour = "gray50"),
            panel.grid.major = element_blank(),
            axis.text.x = element_text(size =10),
            axis.text.y =element_text(size =10),
            legend.position = "bottom", 
            legend.title = element_text(size=8), #change legend title font size
            legend.text = element_text(size=8)) #change legend text font size
    
    chart_list[[n]] <- ggimg3
  }
  }
  
  #Creating Table data file

  f.dis_tot_pct <- f.dis_chart_tab %>% filter(dis_AGE == "TOT") %>% filter(TYPE == 1) %>%
           mutate(EST_TOT_PCT = EST_PCT/100,
                  MOE_TOT_PCT = MOE_PCT/100) %>% 
           select(GEOID,  EST_TOT_PCT, MOE_TOT_PCT)
  f.dis_pov_pct <- f.dis_chart_tab %>% filter(dis_AGE == "TOT") %>% filter(TYPE == 0) %>%
    mutate(EST_POV_PCT = EST_PCT/100,
           MOE_POV_PCT = MOE_PCT/100) %>%
    select(GEOID, EST_POV_PCT, MOE_POV_PCT)
    
  
  f.dis_pct <- inner_join(f.dis_tot_pct, f.dis_pov_pct, by="GEOID") %>%
            inner_join(., f.age_sya, by="GEOID") %>%
          mutate(DIS_TOT_EST = TOTAL_EST * EST_TOT_PCT,
                 DIS_TOT_MOE = TOTAL_EST * MOE_TOT_PCT,
                 DIS_POV_EST = DIS_TOT_EST * EST_POV_PCT,
                 DIS_POV_MOE = DIS_TOT_MOE * EST_POV_PCT) %>%
    select(GEOID, DIS_TOT_EST, DIS_TOT_MOE, DIS_POV_EST, DIS_POV_MOE)
  
  f.dis_tab <- inner_join(f.dis_chart_tab, f.dis_pct, by ="GEOID") %>% 
    mutate(N_EST = ifelse(TYPE == 0, ((EST_PCT/100)*DIS_POV_EST),((EST_PCT/100) * DIS_TOT_EST)),
           N_MOE = ifelse(TYPE == 0, ((MOE_PCT/100)*DIS_POV_MOE),((MOE_PCT/100) * DIS_TOT_MOE))) %>%
    select(GEOID, NAME, TYPE, dis_AGE,N_EST, N_MOE, EST_PCT, MOE_PCT)
      

  f.dis_tab_fin <- f.dis_tab %>%
    mutate(N_EST = comma_conv(N_EST),
           N_MOE = comma_conv(N_MOE),
           EST_PCT = percent(EST_PCT),
           MOE_PCT = percent(MOE_PCT))
  
  f.dis_tab_fin$dis_AGE <- factor(f.dis_tab_fin$dis_AGE, 
                                  levels = c("LE18",  "1864", "GE65", "TOT"))
  
  f.dis_tab_fin_l <- f.dis_tab_fin %>% arrange(GEOID, factor(dis_AGE)) %>%  
    pivot_wider(names_from =dis_AGE, values_from =c(N_EST, N_MOE, EST_PCT, MOE_PCT)) %>%
    arrange(GEOID) %>%
    mutate(NAME = ifelse(TYPE == 1,"",NAME)) %>%
    select("NAME", "TYPE",
           "N_EST_LE18",	"N_MOE_LE18",	"EST_PCT_LE18",	"MOE_PCT_LE18",
           "N_EST_1864",	"N_MOE_1864",	"EST_PCT_1864",	"MOE_PCT_1864",
           "N_EST_GE65",	"N_MOE_GE65",	"EST_PCT_GE65",	"MOE_PCT_GE65",
           "N_EST_TOT",	"N_MOE_TOT",	"EST_PCT_TOT",	"MOE_PCT_TOT")
  
  f.dis_tab_fin_l$TYPE <- factor(f.dis_tab_fin_l$TYPE, levels=c(0,1),
                                 labels =c("Below Federal Poverty Level",
                                           "Total Population"))
  
  # Flex Table
  tab_head <- paste0("Persons with Disabilities by Age and Poverty Status ",listID$plName1)
  head_level <- c("","","Age Less Than 18","","","",
                  "Age 18 to 64","","","",
                  "Age 65 and Older","","","",
                  "All Persons with Disabilities","","","")
  head_measure <- c("CSBG Entity","Poverty Status",rep(c("Estimate","Margin of Error"),8))
  
  headers <-t(bind_cols(as.data.frame(head_level), as.data.frame(head_measure)))
  
  f.disFlex <- flextable(
    f.dis_tab_fin_l,
    col_keys = names(f.dis_tab_fin_l)) %>%
    add_header_row(values =head_measure,top =TRUE) %>%
    add_header_row(values =head_level,top =TRUE) %>%
    align(j =1:18,align ="center",part ="header") %>%
    delete_rows(i = 3, part = "header") %>%
    merge_at(i =1,j =3:6, part ="header") %>%
    merge_at(i =1,j =7:10, part ="header") %>%
    merge_at(i =1,j =11:14, part ="header") %>%
    merge_at(i =1,j =15:18, part ="header") %>%
    align(j =1:2, align ="left", part ="body") %>%
    align(j =3:18, align ="right", part ="body") %>%
    width(j =1:2, width =4) %>%
    width(j =3:18,width =1) 
  
  f.disFlex <- add_header_lines(f.disFlex,values =tab_head, top =TRUE)
  
  
  
  #bind list
  outList <- list("plot" = chart_list, "data" =  f.dis_tab_fin_l, "header_sh" = headers, "FlexTable" = f.disFlex)
  
  return(outList)
}
