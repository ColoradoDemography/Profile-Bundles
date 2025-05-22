#' educPRO Creates a Chart comparing educational attainment of two areas
#'
#' Modified from ms_ed in codemogProfile AB 12/2017
# 'Produces plotly plot, data and table for selected CSBG agencies 10/2019
#'
#' @param lvl is the data type ("Regional Summary" or "Counties)
#' @param listID is the list of selected county codes
#' @param curYr is the single year value to be extracted by county_sya
#' @return plotly graphic, data table and data file
#' @export
educPRO <- function(listID, curYr, inData, barCol, barCol2){

  # Collecting List of Counties
  outCap <- paste0(captionSrc("ACS",curYr,"B17003"),"\nPopulation Age 25 and Older") 
  cty_str <- listID$list2
  full_str <- c("01000","08000",unlist(cty_str))
  cty_str2 <- c("08000",unlist(cty_str))
  cty_name <- CountyName(cty_str)
  cty_num <- as.numeric(str_sub(cty_str,3,5))
  name1 <- listID$plName1
  


  
  f.USage_25 <- inData[["US"]] %>%  filter(AGE != 999) %>% filter(AGE >= 25) %>%
    filter(SEX == 0) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    mutate(countyname = "United States",
           year = curYr,
           GEOID ='01000') %>%
    select(GEOID, countyname, year, totalpopulation)
  
  
  
  
  f.COAge <- inData[["CO"]] %>% filter(GEOID %in% cty_str2) %>%
    filter(age >= 25) %>%
    group_by(GEOID, countyname) %>%
    summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
    mutate(year = curYr) %>%
    ungroup() %>%
    select(GEOID, countyname, year, totalpopulation)
  
  if(length(cty_str) > 1) {
    f.csbgAge <- f.COAge %>% filter(GEOID %in% cty_str) %>%
      summarize(totalpopulation = sum(totalpopulation, na.rm=TRUE)) %>%
      mutate(countyname = name1,
             year = curYr,
             GEOID ='08000.5') %>%
      ungroup() %>%
      select(GEOID, countyname, year, totalpopulation)
    
    f.age_fin <- bind_rows(f.USage_25, f.csbgAge, f.COAge)
  } else {
    f.age_fin <- bind_rows(f.USage_25, f.COAge)
  }
  
  

  # POVerty US
  f.B17003_ACS<- inData[["B17003"]] %>% filter(GEOID %in% full_str) %>%
    mutate(
      TOT_TOT_EST = .[[3]],	
      TOT_TOT_MOE = .[[4]]^2,
      TOT_POV_EST = .[[5]],	
      TOT_POV_MOE = .[[6]]^2,
      LTHSGRAD_POV_EST = .[[9]] + .[[19]],	
      LTHSGRAD_POV_MOE = .[[10]]^2 + .[[20]]^2,
      HSGRAD_POV_EST = .[[11]] + .[[21]],	
      HSGRAD_POV_MOE = .[[12]]^2 + .[[22]]^2,
      SCOLL_POV_EST = .[[13]] + .[[23]],	
      SCOLL_POV_MOE = .[[14]]^2 + .[[24]]^2,
      BA_POV_EST = .[[15]] + .[[25]],	
      BA_POV_MOE = .[[16]]^2 + .[[26]]^2,
      LTHSGRAD_TOT_EST = .[[9]] + .[[19]] + .[[31]] + .[[41]],	
      LTHSGRAD_TOT_MOE = .[[10]]^2 + .[[20]]^2 + .[[32]]^2 + .[[42]]^2,
      HSGRAD_TOT_EST = .[[11]] + .[[21]] + .[[33]] + .[[43]],	
      HSGRAD_TOT_MOE = .[[12]]^2 + .[[22]]^2 + .[[34]]^2 + .[[44]]^2,
      SCOLL_TOT_EST = .[[13]] + .[[23]] + .[[35]] + .[[45]],	
      SCOLL_TOT_MOE = .[[14]]^2 + .[[24]]^2 + .[[36]]^2 + .[[46]]^2,
      BA_TOT_EST = .[[15]] + .[[25]] + .[[37]] + .[[47]],	
      BA_TOT_MOE = .[[16]]^2 + .[[26]]^2 + .[[38]]^2 + .[[48]]^2
    )  %>%
    select(GEOID, NAME, TOT_TOT_EST : BA_TOT_MOE)

  if(length(cty_str) > 1) {  #Summarize for Multi County Place
    f.B17003_csbg <- f.B17003_ACS %>% filter(GEOID %in% cty_str) %>%
      summarise(TOT_TOT_EST = sum(TOT_TOT_EST, na.rm =TRUE),
                TOT_TOT_MOE = sum(TOT_TOT_MOE, na.rm =TRUE),
                TOT_POV_EST = sum(TOT_POV_EST, na.rm =TRUE),
                TOT_POV_MOE = sum(TOT_POV_MOE, na.rm =TRUE),
                LTHSGRAD_POV_EST = sum(LTHSGRAD_POV_EST, na.rm =TRUE),
                LTHSGRAD_POV_MOE = sum(LTHSGRAD_POV_MOE, na.rm =TRUE),
                HSGRAD_POV_EST = sum(HSGRAD_POV_EST, na.rm =TRUE),
                HSGRAD_POV_MOE = sum(HSGRAD_POV_MOE, na.rm =TRUE),
                SCOLL_POV_EST = sum(SCOLL_POV_EST, na.rm =TRUE),
                SCOLL_POV_MOE = sum(SCOLL_POV_MOE, na.rm =TRUE),
                BA_POV_EST = sum(BA_POV_EST, na.rm =TRUE),
                BA_POV_MOE = sum(BA_POV_MOE, na.rm =TRUE),
                LTHSGRAD_TOT_EST = sum(LTHSGRAD_TOT_EST, na.rm =TRUE),
                LTHSGRAD_TOT_MOE = sum(LTHSGRAD_TOT_MOE, na.rm =TRUE),
                HSGRAD_TOT_EST = sum(HSGRAD_TOT_EST, na.rm =TRUE),
                HSGRAD_TOT_MOE = sum(HSGRAD_TOT_MOE, na.rm =TRUE),
                SCOLL_TOT_EST = sum(SCOLL_TOT_EST, na.rm =TRUE),
                SCOLL_TOT_MOE = sum(SCOLL_TOT_MOE, na.rm =TRUE),
                BA_TOT_EST = sum(BA_TOT_EST, na.rm =TRUE),
                BA_TOT_MOE = sum(BA_TOT_MOE, na.rm =TRUE)) %>%
      mutate(GEOID = "08000.5",
             NAME = name1) %>%
      select(GEOID, NAME, TOT_TOT_EST : BA_TOT_MOE)
    
    f.B17003_US <- f.B17003_ACS %>% filter(GEOID == "01000")
    f.B17003_CO <- f.B17003_ACS %>% filter(GEOID == "08000")
    f.B17003_CTY <- f.B17003_ACS %>% filter(GEOID %in% cty_str)
    
    f.B17003_FIN <- bind_rows(f.B17003_US, f.B17003_CO, f.B17003_csbg, f.B17003_CTY)
  } else {
    f.B17003_FIN <- f.B17003_ACS %>% filter(GEOID %in% full_str)
  }
  
 
  # calculating Percentage
  f.B17003_FIN <- f.B17003_FIN %>%
    mutate(NAME = str_replace(NAME, ", Colorado",""),
           TOT_TOT_MOE = sqrt(TOT_TOT_MOE),
           TOT_POV_MOE = sqrt(TOT_POV_MOE),
           LTHSGRAD_POV_MOE = sqrt(LTHSGRAD_POV_MOE),
           HSGRAD_POV_MOE = sqrt(HSGRAD_POV_MOE),
           SCOLL_POV_MOE = sqrt(SCOLL_POV_MOE),
           BA_POV_MOE = sqrt(BA_POV_MOE),
           LTHSGRAD_TOT_MOE = sqrt(LTHSGRAD_TOT_MOE),
           HSGRAD_TOT_MOE = sqrt(HSGRAD_TOT_MOE),
           SCOLL_TOT_MOE = sqrt(SCOLL_TOT_MOE),
           BA_TOT_MOE = sqrt(BA_TOT_MOE),
           TOT_POV_EST_PCT = TOT_POV_EST/TOT_TOT_EST,
           TOT_POV_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, TOT_POV_MOE, TOT_POV_EST_PCT),
           LTHSGRAD_POV_EST_PCT = LTHSGRAD_POV_EST/TOT_POV_EST,
           LTHSGRAD_POV_MOE_PCT = pctMOE(TOT_POV_EST, TOT_POV_MOE, LTHSGRAD_POV_MOE, LTHSGRAD_POV_EST_PCT),
           HSGRAD_POV_EST_PCT = HSGRAD_POV_EST/TOT_POV_EST,
           HSGRAD_POV_MOE_PCT = pctMOE(TOT_POV_EST, TOT_POV_MOE, HSGRAD_POV_MOE, HSGRAD_POV_EST_PCT),
           SCOLL_POV_EST_PCT = SCOLL_POV_EST/TOT_POV_EST,
           SCOLL_POV_MOE_PCT = pctMOE(TOT_POV_EST, TOT_POV_MOE, SCOLL_POV_MOE, SCOLL_POV_EST_PCT),
           BA_POV_EST_PCT = BA_POV_EST/TOT_POV_EST,
           BA_POV_MOE_PCT = pctMOE(TOT_POV_EST, TOT_POV_MOE, BA_POV_MOE, BA_POV_EST_PCT),
           LTHSGRAD_TOT_EST_PCT = LTHSGRAD_TOT_EST/TOT_TOT_EST,
           LTHSGRAD_TOT_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, LTHSGRAD_TOT_MOE, LTHSGRAD_TOT_EST_PCT),
           HSGRAD_TOT_EST_PCT = HSGRAD_TOT_EST/TOT_TOT_EST,
           HSGRAD_TOT_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, HSGRAD_TOT_MOE, HSGRAD_TOT_EST_PCT),
           SCOLL_TOT_EST_PCT = SCOLL_TOT_EST/TOT_TOT_EST,
           SCOLL_TOT_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, SCOLL_TOT_MOE, SCOLL_TOT_EST_PCT),
           BA_TOT_EST_PCT = BA_TOT_EST/TOT_TOT_EST,
           BA_TOT_MOE_PCT = pctMOE(TOT_TOT_EST, TOT_TOT_MOE, BA_TOT_MOE, BA_TOT_EST_PCT))  %>% 
        arrange(GEOID) %>%
        select(GEOID, NAME, TOT_TOT_EST : BA_TOT_MOE_PCT)

  
  # Building Chart data sets
  f.educ_est <- f.B17003_FIN %>% select(GEOID, NAME, TOT_POV_EST_PCT, LTHSGRAD_POV_EST_PCT,	HSGRAD_POV_EST_PCT,	SCOLL_POV_EST_PCT,	BA_POV_EST_PCT,	LTHSGRAD_TOT_EST_PCT,	HSGRAD_TOT_EST_PCT,	SCOLL_TOT_EST_PCT,	BA_TOT_EST_PCT) %>%
    gather(EDUC_ATT, EST_PCT,TOT_POV_EST_PCT : BA_TOT_EST_PCT, factor_key =TRUE) %>%
    mutate(EDUC_ATT = str_replace(EDUC_ATT,"_EST_PCT",""))
  
  f.educ_moe <- f.B17003_FIN %>% select(GEOID, NAME, TOT_POV_MOE_PCT, LTHSGRAD_POV_MOE_PCT,	HSGRAD_POV_MOE_PCT,	SCOLL_POV_MOE_PCT,	BA_POV_MOE_PCT,	LTHSGRAD_TOT_MOE_PCT,	HSGRAD_TOT_MOE_PCT,	SCOLL_TOT_MOE_PCT,	BA_TOT_MOE_PCT) %>%
    gather(EDUC_ATT, MOE_PCT, TOT_POV_MOE_PCT : BA_TOT_MOE_PCT, factor_key =TRUE) %>%
    mutate(EDUC_ATT = str_replace(EDUC_ATT,"_MOE_PCT","")) %>% select(-NAME)
  

  f.educ_chart <- inner_join(f.educ_est, f.educ_moe, by =c('GEOID',"EDUC_ATT")) %>%
    mutate(EST_PCT = EST_PCT * 100,
           MOE_PCT = MOE_PCT * 100,
           TYPE = ifelse(grepl("_POV",EDUC_ATT),0,1),
           EDUC_ATT = str_replace(EDUC_ATT,"_POV",""),
           EDUC_ATT = str_replace(EDUC_ATT,"_TOT",""))
  f.educ_chart_tab <- f.educ_chart
  
  f.educ_chart <- f.educ_chart %>% filter(EDUC_ATT != "TOT")
  f.educ_chart$EDUC_ATT <- factor(f.educ_chart$EDUC_ATT, 
                                  levels = c("LTHSGRAD",  "HSGRAD", "SCOLL", "BA"),
                                  labels = c("Less than\nHigh School Graduate",
                                             "High School Graduate",
                                             "Some College/\nAssociate's Degree",
                                             "Bachelor's Degree or Higher"))
  
  f.educ_chart$TYPE <- factor(f.educ_chart$TYPE, levels=c(0,1),
                              labels =c("Below Federal Poverty Level",
                                        "Total Population"))
  
  nameList <- unique(f.educ_chart$NAME)
  f.educ_chart$NAME <- factor(f.educ_chart$NAME, levels =nameList)
  
  chart_list <- list()
  pltTitle <- paste0("Educational Attainment by Poverty Status: ", name1)
  
  if(length(cty_str) > 1) {
    f.educ_chart3 <- f.educ_chart %>% filter(!(GEOID %in% cty_str))
  } else {
    f.educ_chart3 <- f.educ_chart
  }
  
  maxLim <- max(f.educ_chart$EST_PCT) + 10

  # Creating Bar Charts
  ggimg3 <-ggplot( f.educ_chart3, aes(x =as.factor(EDUC_ATT), y =EST_PCT, fill = NAME)) +
    geom_bar(stat ="identity", position = "dodge", color ="black") + 
    geom_text(aes(x =as.factor(EDUC_ATT), y =EST_PCT, label =percent(EST_PCT), group = NAME), 
              position = position_dodge(width =1.0),
              vjust =-0.8,  size =4) +
    scale_fill_manual(values =barCol) +
    scale_y_continuous(limits = c(0, maxLim), label =percent, expand = c(0, 0)) +
    facet_wrap(vars(TYPE), ncol=1) +
    labs(title = pltTitle,
         subtitle = "CSBG Comparison",
         caption = outCap,
         x = "Educational Attainment",
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
  
  chart_list[[1]] <- ggimg3
  
  if(length(cty_str) > 1) {
  for(i in 1:length(cty_str)) {
    n <- i + 1
    f.chart_data <- f.educ_chart %>% filter(GEOID == "08000.5" | GEOID == cty_str[i]) 
   
    ggimg3 <-ggplot( f.chart_data, aes(x =as.factor(EDUC_ATT), y =EST_PCT, fill = NAME)) +
      geom_bar(stat ="identity", position = "dodge", color ="black") + 
      geom_text(aes(x =as.factor(EDUC_ATT), y =EST_PCT, label =percent(EST_PCT), group = NAME), 
                position = position_dodge(width =1.0),
                vjust =-0.8,  size =4) +
      scale_fill_manual(values =barCol2) +
      scale_y_continuous(limits = c(0, maxLim), label =percent, expand = c(0, 0)) +
      facet_wrap(vars(TYPE), ncol=1) +
      labs(title = pltTitle,
           subtitle = "County Comparison",
           caption = outCap,
           x = "Educational Attainment",
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
  

  #Creating Table data file
  # selecting Total Povery Percentages
  f.educ_pov <- f.educ_chart_tab %>% filter(EDUC_ATT == "TOT") %>%
       mutate(EST_PCT_POV = EST_PCT/100,
              MOE_PCT_POV = MOE_PCT/100) %>%
       select(GEOID, EST_PCT_POV, MOE_PCT_POV)
  f.educ_tab <- inner_join(f.educ_chart_tab, f.age_fin, by ="GEOID") %>% 
                inner_join(., f.educ_pov, by="GEOID") %>%
    filter(EDUC_ATT != "TOT") %>%
    mutate(N_EST = ifelse(TYPE == 0, ((EST_PCT/100)*(EST_PCT_POV * totalpopulation)),(EST_PCT/100) * totalpopulation),
           N_MOE = ifelse(TYPE == 0, ((MOE_PCT/100)*(MOE_PCT_POV * totalpopulation)),(MOE_PCT/100) * totalpopulation)) %>%
    select(GEOID, NAME, year, TYPE, EDUC_ATT,N_EST, N_MOE, EST_PCT, MOE_PCT)
  
  f.educ_tab_tot <- f.educ_tab %>%
    group_by(GEOID, NAME, TYPE) %>%
    summarize(N_EST = sum(N_EST),
              N_MOE = sum(N_MOE),
              EST_PCT = sum(EST_PCT),
              MOE_PCT = sum(MOE_PCT)) %>%
    mutate(EDUC_ATT = "TOTAL",
           year = curYr) %>%
    select(GEOID, NAME, year, TYPE, EDUC_ATT, N_EST, N_MOE, EST_PCT, MOE_PCT)
  
  f.educ_tab_fin <- bind_rows(f.educ_tab, f.educ_tab_tot) %>%
    mutate(N_EST = comma_conv(N_EST),
           N_MOE = comma_conv(N_MOE),
           EST_PCT = percent(EST_PCT),
           MOE_PCT = percent(MOE_PCT))
  
  f.educ_tab_fin$EDUC_ATT <- factor(f.educ_tab_fin$EDUC_ATT, 
                                    levels = c("LTHSGRAD",  "HSGRAD", "SCOLL", "BA", "TOTAL"))
  
  f.educ_tab_fin_l <- f.educ_tab_fin %>% arrange(GEOID, factor(EDUC_ATT)) %>%
    pivot_wider(names_from =EDUC_ATT, values_from =c(N_EST, N_MOE, EST_PCT, MOE_PCT)) %>%
    arrange(GEOID) %>%
    mutate(NAME = ifelse(TYPE == 1,"",NAME)) %>%
    select("NAME", "TYPE",
           "N_EST_LTHSGRAD",	"N_MOE_LTHSGRAD",	"EST_PCT_LTHSGRAD",	"MOE_PCT_LTHSGRAD",
           "N_EST_HSGRAD",	"N_MOE_HSGRAD",	"EST_PCT_HSGRAD",	"MOE_PCT_HSGRAD",
           "N_EST_SCOLL",	"N_MOE_SCOLL",	"EST_PCT_SCOLL",	"MOE_PCT_SCOLL",
           "N_EST_BA",	"N_MOE_BA",	"EST_PCT_BA",	"MOE_PCT_BA",
           "N_EST_TOTAL",	"N_MOE_TOTAL",	"EST_PCT_TOTAL",	"MOE_PCT_TOTAL")
  

  
  f.educ_tab_fin_l$TYPE <- factor(f.educ_tab_fin_l$TYPE, levels=c(0,1),
                              labels =c("Below Federal Poverty Level",
                                        "Total Population"))
  
  # Flex Table
  tab_head <- paste0("Educational Attainment by Poverty Status, ",listID$plName1)
  head_level <- c("","","Less Than High School Graduate","","","",
                  "High School Graduate","","","",
                  "Some College/Associate's Degree","","","",
                  "Bachelor's Degree or Higher","","","",
                  "Total","","","")
  head_measure <- c("CSBG Entity","Poverty Status",rep(c("Estimate","Margin of Error"),10))
  
  headers <-t(bind_cols(as.data.frame(head_level), as.data.frame(head_measure)))
  
  f.educFlex <- flextable(
    f.educ_tab_fin_l,
    col_keys = names(f.educ_tab_fin_l)) %>%
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
  
  f.educFlex <- add_header_lines(f.educFlex,values =tab_head, top =TRUE)
  
  
  
  #bind list
  outList <- list("plot" = chart_list, "data" =  f.educ_tab_fin_l, "header_sh" = headers, "FlexTable" = f.educFlex)
  
  return(outList)
}
