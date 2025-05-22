#' snap generates tables and plotly charts for Supplemental Nutrition Assistance Program (SNAP)/Food Stamps
#'   data from Hunger Free Colorado
#'  CSBG Dashboard 11/2019 REVISED  9/2022 A. Bickford
#' @param DBPool the DOLA database pool
#' @param lvl the selected agency
#' @param listID is the list of selected county codes
#' @param cyrYr Current year value
#' @return plotly graphic, data table and data file
#' @export

snap <- function(listID, curYr, inData, barCol, barCol2){

  # Collecting List of Counties
  outCap <- captionSrc("ACS",curYr,"B22003")
  cty_str <- listID$list2
  full_str <- c("01000","08000",unlist(cty_str))
  cty_str2 <- c("08000",unlist(cty_str))
  cty_name <- CountyName(cty_str)
  cty_num <- as.numeric(str_sub(cty_str,3,5))
  name1 <- listID$plName1
  
  
  # This is a family table, no adjustment for population estimates
  

  # POVerty US
  f.B22003_ACS <- inData[["B22003"]] %>% filter(GEOID %in% full_str) %>%
    mutate(
      TOT_TOT_EST = .[[3]],	
      TOT_TOT_MOE = .[[4]]^2,
      SNAP_TOT_EST = .[[5]],	
      SNAP_TOT_MOE = .[[6]]^2,
      SNAP_POV_EST = .[[7]],	
      SNAP_POV_MOE = .[[8]]^2,
      SNAP_NONPOV_EST = .[[9]],	
      SNAP_NONPOV_MOE = .[[10]]^2,
      NONSNAP_TOT_EST = .[[11]],	
      NONSNAP_TOT_MOE = .[[12]]^2,
      NONSNAP_POV_EST = .[[13]],	
      NONSNAP_POV_MOE = .[[14]]^2,
      NONSNAP_NONPOV_EST = .[[15]],	
      NONSNAP_NONPOV_MOE = .[[16]]^2,
      TOT_POV_EST = .[[7]] + .[[13]],	
      TOT_POV_MOE = .[[8]]^2 + .[[14]]^2,
      TOT_NONPOV_EST = .[[9]] + .[[15]],	
      TOT_NONPOV_MOE = .[[10]]^2 + .[[16]]^2
    )  %>%
    select(GEOID, NAME, TOT_TOT_EST : TOT_NONPOV_MOE)
  
  
  if(length(cty_str) > 1) {  #Summarize for Multi County Place
    f.B22003_csbg <- f.B22003_ACS %>% filter(GEOID %in% cty_str) %>%
      summarise(TOT_TOT_EST = sum(TOT_TOT_EST, na.rm=TRUE),	
                TOT_TOT_MOE = sum(TOT_TOT_MOE, na.rm=TRUE),
                SNAP_TOT_EST = sum(SNAP_TOT_EST, na.rm=TRUE),	
                SNAP_TOT_MOE = sum(SNAP_TOT_MOE, na.rm=TRUE),
                SNAP_POV_EST = sum(SNAP_POV_EST, na.rm=TRUE),	
                SNAP_POV_MOE = sum(SNAP_POV_MOE, na.rm=TRUE),
                SNAP_NONPOV_EST = sum(SNAP_NONPOV_EST, na.rm=TRUE),	
                SNAP_NONPOV_MOE = sum(SNAP_NONPOV_MOE, na.rm=TRUE),
                NONSNAP_TOT_EST = sum(NONSNAP_TOT_EST, na.rm=TRUE),	
                NONSNAP_TOT_MOE = sum(NONSNAP_TOT_MOE, na.rm=TRUE),
                NONSNAP_POV_EST = sum(NONSNAP_POV_EST, na.rm=TRUE),	
                NONSNAP_POV_MOE = sum(NONSNAP_POV_MOE, na.rm=TRUE),
                NONSNAP_NONPOV_EST = sum(NONSNAP_NONPOV_EST, na.rm=TRUE),	
                NONSNAP_NONPOV_MOE = sum(NONSNAP_NONPOV_MOE, na.rm=TRUE),
                TOT_POV_EST = sum(TOT_POV_EST, na.rm=TRUE),	
                TOT_POV_MOE = sum(TOT_POV_MOE, na.rm=TRUE),
                TOT_NONPOV_EST = sum(TOT_NONPOV_EST, na.rm=TRUE),	
                TOT_NONPOV_MOE = sum(TOT_NONPOV_MOE, na.rm=TRUE)
      ) %>%
      mutate(GEOID = "08000.5",
             NAME = name1) %>%
      select(GEOID, NAME, TOT_TOT_EST : TOT_NONPOV_MOE)
    
    f.B22003_US <- f.B22003_ACS %>% filter(GEOID == "01000")
    f.B22003_CO <- f.B22003_ACS %>% filter(GEOID == "08000")
    f.B22003_CTY <- f.B22003_ACS %>% filter(GEOID %in% cty_str)
    
    f.B22003_FIN <- bind_rows(f.B22003_US, f.B22003_CO, f.B22003_csbg, f.B22003_CTY)
  } else {
    f.B22003_FIN <- f.B22003_ACS 
  }

  # calculating Percentage
  f.B22003_FIN <- f.B22003_FIN %>%
    mutate(NAME = str_replace(NAME, ", Colorado",""),
           TOT_TOT_MOE = sqrt(TOT_TOT_MOE),
           SNAP_TOT_MOE = sqrt(SNAP_TOT_MOE),
           SNAP_POV_MOE = sqrt(SNAP_POV_MOE),
           SNAP_NONPOV_MOE = sqrt(SNAP_NONPOV_MOE),
           NONSNAP_TOT_MOE = sqrt(NONSNAP_TOT_MOE),
           NONSNAP_POV_MOE = sqrt(NONSNAP_POV_MOE),
           NONSNAP_NONPOV_MOE = sqrt(NONSNAP_NONPOV_MOE),
           TOT_POV_MOE = sqrt(TOT_POV_MOE),
           TOT_NONPOV_MOE = sqrt(TOT_NONPOV_MOE),
           
           SNAP_TOT_EST_PCT = SNAP_TOT_EST/TOT_TOT_EST,
           SNAP_TOT_MOE_PCT = pctMOE(TOT_TOT_EST,TOT_TOT_MOE,SNAP_TOT_MOE,SNAP_TOT_EST_PCT),
           SNAP_POV_EST_PCT = SNAP_POV_EST/TOT_POV_EST,
           SNAP_POV_MOE_PCT = pctMOE(TOT_POV_EST,TOT_POV_MOE,SNAP_POV_MOE,SNAP_POV_EST_PCT),
           NONSNAP_POV_EST_PCT = NONSNAP_POV_EST/TOT_POV_EST,
           NONSNAP_POV_MOE_PCT = pctMOE(TOT_POV_EST,TOT_POV_MOE,NONSNAP_POV_MOE,NONSNAP_POV_EST_PCT),
           NONSNAP_NONPOV_EST_PCT = NONSNAP_NONPOV_EST/NONSNAP_TOT_EST,
           SNAP_TOT_EST_PCT = SNAP_TOT_EST/TOT_TOT_EST,
           SNAP_TOT_MOE_PCT = pctMOE(TOT_TOT_EST,TOT_TOT_MOE,SNAP_TOT_MOE,SNAP_TOT_EST_PCT),
           NONSNAP_TOT_EST_PCT = NONSNAP_TOT_EST/TOT_TOT_EST,
           NONSNAP_TOT_MOE_PCT = pctMOE(TOT_TOT_EST,TOT_TOT_MOE,NONSNAP_TOT_MOE,NONSNAP_TOT_EST_PCT),
           NONSNAP_NONPOV_MOE_PCT = pctMOE(NONSNAP_TOT_EST,NONSNAP_TOT_MOE,NONSNAP_NONPOV_MOE,NONSNAP_POV_EST_PCT),
           TOT_POV_EST_PCT = TOT_POV_EST/TOT_TOT_EST,
           TOT_POV_MOE_PCT = pctMOE(TOT_TOT_EST,TOT_TOT_MOE,TOT_POV_MOE,TOT_POV_EST_PCT)
           
    )  %>% 
    select(GEOID, NAME, SNAP_POV_EST,	SNAP_POV_MOE, SNAP_POV_EST_PCT,	SNAP_POV_MOE_PCT,
           NONSNAP_POV_EST,	NONSNAP_POV_MOE, NONSNAP_POV_EST_PCT,	NONSNAP_POV_MOE_PCT,
           SNAP_TOT_EST,	SNAP_TOT_MOE, SNAP_TOT_EST_PCT,	SNAP_TOT_MOE_PCT, 
           NONSNAP_TOT_EST,	NONSNAP_TOT_MOE, NONSNAP_TOT_EST_PCT,	NONSNAP_TOT_MOE_PCT,
           TOT_POV_EST,	TOT_POV_MOE, TOT_POV_EST_PCT,	TOT_POV_MOE_PCT, TOT_TOT_EST,	TOT_TOT_MOE)
  
  
  # Building Chart data sets
  f.snap_est <- f.B22003_FIN %>% select(GEOID, NAME, 
                                       SNAP_POV_EST_PCT,	NONSNAP_POV_EST_PCT,	SNAP_TOT_EST_PCT,	NONSNAP_TOT_EST_PCT) %>%
    gather(SNAP_STAT, EST_PCT,SNAP_POV_EST_PCT : NONSNAP_TOT_EST_PCT, factor_key =TRUE) %>%
    mutate(SNAP_STAT = str_replace(SNAP_STAT,"_EST_PCT",""))
  
  f.snap_moe <- f.B22003_FIN %>% select(GEOID, NAME, 
                                       SNAP_POV_MOE_PCT,	NONSNAP_POV_MOE_PCT,	SNAP_TOT_MOE_PCT,	NONSNAP_TOT_MOE_PCT) %>%
    gather(SNAP_STAT, MOE_PCT,SNAP_POV_MOE_PCT : NONSNAP_TOT_MOE_PCT, factor_key =TRUE) %>%
    mutate(SNAP_STAT = str_replace(SNAP_STAT,"_MOE_PCT","")) %>% select(-NAME)
  

  f.snap_chart <- inner_join(f.snap_est, f.snap_moe, by =c('GEOID',"SNAP_STAT"))   %>%
    mutate(EST_PCT = EST_PCT * 100,
           MOE_PCT = MOE_PCT * 100,
           TYPE = ifelse(grepl("POV", SNAP_STAT),0,1),
           SNAP_STAT = str_replace(SNAP_STAT,"_POV",""),
           SNAP_STAT = str_replace(SNAP_STAT,"_TOT","")) %>% replace(is.na(.),0)
  f.snap_chart_tab <- f.snap_chart
  
  
  f.snap_chart$SNAP_STAT <- factor(f.snap_chart$SNAP_STAT, 
                                 levels = c("SNAP",  "NONSNAP"),
                                 labels = c("Received SNAP Benefits",
                                            "Did not receive SNAP Benefits"))
  
  f.snap_chart$TYPE <- factor(f.snap_chart$TYPE, levels=c(0,1),
                             labels =c("Below Federal Poverty Level",
                                       "Total Population"))
  
  nameList <- unique(f.snap_chart$NAME)
  f.snap_chart$NAME <- factor(f.snap_chart$NAME, levels =nameList)
  
  chart_list <- list()
  pltTitle <- paste0("Received SNAP Benebits by Poverty Level: ", name1)
  
  
  if(length(cty_str) > 1) {
    f.snap_chart3 <- f.snap_chart %>% filter(!(GEOID %in% cty_str))
  } else {
    f.snap_chart3 <- f.snap_chart
  }
 
  maxLim <- max(f.snap_chart$EST_PCT) + 10
  
  # Creating Bar Charts
  ggimg3 <-ggplot( f.snap_chart3, aes(x =as.factor(SNAP_STAT), y =EST_PCT, fill = NAME)) +
    geom_bar(stat ="identity", position = "dodge", color ="black") + 
    geom_text(aes(x =as.factor(SNAP_STAT), y =EST_PCT, label =percent(EST_PCT), group = NAME), 
              position = position_dodge(width =1.0),
              vjust =-0.8, size = 3) +
    scale_fill_manual(values =barCol) +
    scale_y_continuous(limits = c(0, maxLim), label =percent, expand = c(0, 0)) +
    facet_wrap(vars(TYPE), ncol=1) +
    labs(title = pltTitle,
         subtitle = "CSBG Comparison",
         caption = outCap,
         x = "SNAP Benefits",
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
    f.chart_data <- f.snap_chart %>% filter(GEOID == "08000.5" | GEOID == cty_str[i]) 
    
    ggimg3 <-ggplot(f.chart_data, aes(x =as.factor(SNAP_STAT), y =EST_PCT, fill = NAME)) +
      geom_bar(stat ="identity", position = "dodge", color ="black") + 
      geom_text(aes(x =as.factor(SNAP_STAT), y =EST_PCT, label =percent(EST_PCT), group = NAME), 
                position = position_dodge(width =1.0),
                vjust =-0.8, size = 3) +
      scale_fill_manual(values =barCol2) +
      scale_y_continuous(limits = c(0, maxLim), label =percent, expand = c(0, 0)) +
      facet_wrap(vars(TYPE), ncol=1) +
      labs(title = pltTitle,
           subtitle = "County Comparison",
           caption = outCap,
           x = "SNAP Benefits",
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
    
    chart_list[[n]] <- ggimg3
  }
  }
  
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
  
  f.B22003_ADJ <- inner_join(f.B22003_FIN, f.ctypop_fin, by="GEOID") %>%
               mutate(TOT_TOT_POV_EST_ADJ = totalpopulation * TOT_POV_EST_PCT,
                      TOT_TOT_POV_MOE_ADJ = totalpopulation *	TOT_POV_MOE_PCT, 
                      TOT_TOT_EST_ADJ = totalpopulation,	
                      TOT_TOT_MOE_ADJ = 0,
                      SNAP_POV_EST_ADJ = TOT_TOT_POV_EST_ADJ * SNAP_POV_EST_PCT,
                      SNAP_POV_MOE_ADJ = TOT_TOT_POV_MOE_ADJ * SNAP_POV_MOE_PCT,
                      NONSNAP_POV_EST_ADJ = TOT_TOT_POV_EST_ADJ * NONSNAP_POV_EST_PCT,
                      NONSNAP_POV_MOE_ADJ = TOT_TOT_POV_MOE_ADJ * NONSNAP_POV_MOE_PCT,
                      SNAP_TOT_EST_ADJ = totalpopulation * SNAP_TOT_EST_PCT,
                      SNAP_TOT_MOE_ADJ = totalpopulation * SNAP_TOT_MOE_PCT,
                      NONSNAP_TOT_EST_ADJ = totalpopulation * NONSNAP_TOT_EST_PCT,
                      NONSNAP_TOT_MOE_ADJ = totalpopulation * NONSNAP_TOT_MOE_PCT)
  

  if(length(cty_name) > 1) {
    name_list <- unique(c("United States","Colorado",name1,cty_name))
  } else {
    name_list <- c("United States","Colorado", cty_name)
  }
  
  f.B22003_ADJ$NAME <- factor(f.B22003_ADJ$NAME ,levels=name_list)
  
  f.snap_pov <- f.B22003_ADJ %>% 
    mutate(TYPE = 0) %>%
    select(GEOID, NAME, TYPE, 
           SNAP_POV_EST_ADJ,	SNAP_POV_MOE_ADJ,	SNAP_POV_EST_PCT,	SNAP_POV_MOE_PCT,	NONSNAP_POV_EST_ADJ,	
           NONSNAP_POV_MOE_ADJ,	NONSNAP_POV_EST_PCT,	NONSNAP_POV_MOE_PCT, TOT_TOT_POV_EST_ADJ,	TOT_TOT_POV_MOE_ADJ)
  names(f.snap_pov) <- c("GEOID", "NAME",	"TYPE",	
                         "SNAP_EST",	"SNAP_MOE",	"SNAP_EST_PCT",	"SNAP_MOE_PCT",	"NONSNAP_EST",	
                         "NONSNAP_MOE",	"NONSNAP_EST_PCT",	"NONSNAP_MOE_PCT", "TOT_EST",	"TOT_MOE")
  
  
  f.snap_tot <- f.B22003_ADJ %>% 
    mutate(TYPE = 1) %>%
    select(GEOID, NAME, TYPE, 
           SNAP_TOT_EST_ADJ,  SNAP_TOT_MOE_ADJ,	SNAP_TOT_EST_PCT,	SNAP_TOT_MOE_PCT,	
           NONSNAP_TOT_EST_ADJ,	NONSNAP_TOT_MOE_ADJ,	
           NONSNAP_TOT_EST_PCT,	NONSNAP_TOT_MOE_PCT,	TOT_TOT_EST_ADJ,	TOT_TOT_MOE_ADJ)
  names(f.snap_tot) <- c("GEOID", "NAME",	"TYPE",	
                         "SNAP_EST",	"SNAP_MOE",	"SNAP_EST_PCT",	"SNAP_MOE_PCT",	"NONSNAP_EST",	
                         "NONSNAP_MOE",	"NONSNAP_EST_PCT",	"NONSNAP_MOE_PCT", "TOT_EST",	"TOT_MOE")
  
  
  f.snap_tab_ADJ_l <- bind_rows(f.snap_pov, f.snap_tot) %>%
    arrange(NAME, TYPE) %>%
  select(-GEOID) 
  
  f.snap_tab_ADJ_l[,c(3,4,7,8,11,12)] <-
    sapply(f.snap_tab_ADJ_l[,c(3,4,7,8,11,12)], 
           function(x) comma_conv(x))
  f.snap_tab_ADJ_l[,c(5,6,9,10)] <-
    sapply(f.snap_tab_ADJ_l[,c(5,6,9,10)], function(x) percent(x * 100))
  
  f.snap_tab_ADJ_l$NAME <- as.character(f.snap_tab_ADJ_l$NAME)
  for(i in 1:nrow(f.snap_tab_ADJ_l)) {
    if(i %% 2 == 0) {
      f.snap_tab_ADJ_l[i,1] = ""
    }
  }
  
  f.snap_tab_ADJ_l$TYPE <- factor(f.snap_tab_ADJ_l$TYPE, levels=c(0,1),
                                 labels =c("Below Federal Poverty Level",
                                           "Total Population"))
  
  # Flex Table
  tab_head <- paste0("Received SNAP Benefits by Poverty Status, ",listID$plName1)
  head_level1 <- c("",	"",	"Received SNAP Benefits",	"",	"",	"",	
                   "Did not receive SNAP Benefits",	"",	"",	"",	"Total",	"")
  
  head_measure <- c("CSBG Entity","Poverty Status",rep(c("Estimate","Margin of Error"),5))
  
  headers <-t(bind_cols(as.data.frame(head_level1), as.data.frame(head_measure)))

  f.snapFlex <- flextable(
    f.snap_tab_ADJ_l,
    col_keys = names(f.snap_tab_ADJ_l)) %>%
    add_header_row(values =head_measure,top =TRUE) %>%
    add_header_row(values =head_level1,top =TRUE) %>%
    align(j =1:12,align ="center",part ="header") %>%
    delete_rows(i = 3, part = "header") %>%
    merge_at(i =1,j =3:6, part ="header") %>%
    merge_at(i =1,j =7:10, part ="header") %>%
    merge_at(i =1,j =11:12, part ="header") %>%
    align(j =1:2, align ="left", part ="body") %>%
    align(j =3:12, align ="right", part ="body") %>%
    width(j =1:2, width =4) %>%
    width(j =3:12,width =1) 
  
  f.snapFlex <- add_header_lines(f.snapFlex,values =tab_head, top =TRUE)

  #bind list
  outList <- list("plot" = chart_list, "data" =  f.snap_tab_ADJ_l, "header_sh" = headers, "FlexTable" = f.snapFlex)
  
  return(outList)
}
