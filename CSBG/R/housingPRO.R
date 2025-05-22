#'housingPRO Outputs Tables and plots for housing tenure by FPL and Headship
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
housingPRO <- function(listID,curYr, inData, barCol, barCol2){
  
  # Collecting List of Counties
  outCap <- captionSrc("ACS",curYr,"B17019")
  cty_str <- listID$list2
  full_str <- c("01000","08000",unlist(cty_str))
  cty_str2 <- c("08000",unlist(cty_str))
  cty_name <- CountyName(cty_str)
  cty_num <- as.numeric(str_sub(cty_str,3,5))
  name1 <- listID$plName1

  
  # This is a family table, no adjustment for population estimates


  # POVerty US
  f.B17019_ACS <- inData[["B17019"]] %>% filter(GEOID %in% full_str) %>%
    mutate(
      TOT_TOT_TOT_EST =  .[[3]],	
      TOT_TOT_TOT_MOE =  .[[4]]^2,
      POV_TOT_TOT_EST = .[[5]],	
      POV_TOT_TOT_MOE = .[[6]]^2,
      POV_TOT_OWN_EST =  .[[9]] + .[[17]] + .[[23]],	
      POV_TOT_OWN_MOE =  .[[10]]^2 + .[[18]]^2 + .[[24]]^2,
      POV_TOT_RENT_EST =  .[[11]] + .[[19]] + .[[25]],	
      POV_TOT_RENT_MOE =  .[[12]]^2 + .[[20]]^2 + .[[26]]^2,
      POV_MARRIED_TOT_EST = .[[7]],	
      POV_MARRIED_TOT_MOE = .[[8]]^2,
      POV_MARRIED_OWN_EST = .[[9]],	
      POV_MARRIED_OWN_MOE = .[[10]]^2,
      POV_MARRIED_RENT_EST = .[[11]],	
      POV_MARRIED_RENT_MOE = .[[12]]^2,
      POV_MALE_TOT_EST = .[[15]],	
      POV_MALE_TOT_MOE = .[[16]]^2,
      POV_MALE_OWN_EST = .[[17]],	
      POV_MALE_OWN_MOE = .[[18]]^2,
      POV_MALE_RENT_EST = .[[19]],	
      POV_MALE_RENT_MOE = .[[20]]^2,
      POV_FEMALE_TOT_EST = .[[21]],	
      POV_FEMALE_TOT_MOE = .[[22]]^2,
      POV_FEMALE_OWN_EST = .[[23]],	
      POV_FEMALE_OWN_MOE = .[[24]]^2,
      POV_FEMALE_RENT_EST = .[[25]],	
      POV_FEMALE_RENT_MOE = .[[26]]^2,

      TOT_TOT_OWN_EST =  .[[9]] + .[[17]] + .[[23]] + .[[31]] + .[[39]] + .[[ 45]],	
      TOT_TOT_OWN_MOE =  .[[10]]^2 + .[[18]]^2  + .[[24]]^2 + .[[32]]^2 + .[[40]]^2 + .[[ 46]]^2,
      TOT_TOT_RENT_EST =  .[[11]] + .[[19]] + .[[25]] + .[[33]] + .[[41]] + .[[ 47]],	
      TOT_TOT_RENT_MOE =  .[[12]]^2 + .[[20]]^2  + .[[26]]^2 + .[[34]]^2 + .[[42]]^2 + .[[ 48]]^2,
      TOT_MARRIED_TOT_EST =  .[[7]] + .[[29]],	
      TOT_MARRIED_TOT_MOE =  .[[8]]^2 + .[[30]]^2,
      TOT_MARRIED_OWN_EST =  .[[9]] + .[[31]],	
      TOT_MARRIED_OWN_MOE =  .[[10]]^2 + .[[32]]^2,
      TOT_MARRIED_RENT_EST =  .[[11]] + .[[33]],	
      TOT_MARRIED_RENT_MOE =  .[[12]]^2 + .[[34]]^2,
      TOT_MALE_TOT_EST =  .[[15]] + .[[37]],	
      TOT_MALE_TOT_MOE =  .[[16]]^2 + .[[38]]^2,
      TOT_MALE_OWN_EST =  .[[17]] + .[[39]],	
      TOT_MALE_OWN_MOE =  .[[18]]^2 + .[[40]]^2,
      TOT_MALE_RENT_EST =  .[[19]] + .[[41]],	
      TOT_MALE_RENT_MOE =  .[[20]]^2 + .[[42]]^2,
      TOT_FEMALE_TOT_EST =  .[[21]] + .[[43]],	
      TOT_FEMALE_TOT_MOE =  .[[22]]^2 + .[[44]]^2,
      TOT_FEMALE_OWN_EST =  .[[23]] + .[[45]],	
      TOT_FEMALE_OWN_MOE =  .[[24]]^2 + .[[46]]^2,
      TOT_FEMALE_RENT_EST =  .[[25]] + .[[47]],	
      TOT_FEMALE_RENT_MOE =  .[[26]]^2 + .[[48]]^2
    )  %>%
    select(GEOID, NAME, TOT_TOT_TOT_EST : TOT_FEMALE_RENT_MOE)
  

  if(length(cty_str) > 1) {  #Summarize for Multi County Place
    f.B17019_csbg <- f.B17019_ACS %>% filter(GEOID %in% cty_str) %>%
      summarise(TOT_TOT_TOT_EST  = sum(TOT_TOT_TOT_EST , na.rm=TRUE),
                TOT_TOT_TOT_MOE  = sum(TOT_TOT_TOT_MOE , na.rm=TRUE),
                POV_TOT_TOT_EST  = sum(POV_TOT_TOT_EST , na.rm=TRUE),
                POV_TOT_TOT_MOE  = sum(POV_TOT_TOT_MOE , na.rm=TRUE),
                POV_TOT_OWN_EST =  sum(POV_TOT_OWN_EST, na.rm=TRUE),	
                POV_TOT_OWN_MOE =  sum(POV_TOT_OWN_MOE, na.rm=TRUE),
                POV_TOT_RENT_EST =  sum(POV_TOT_RENT_EST, na.rm=TRUE),	
                POV_TOT_RENT_MOE =  sum(POV_TOT_RENT_EST, na.rm=TRUE),
                POV_MARRIED_TOT_EST  = sum(POV_MARRIED_TOT_EST , na.rm=TRUE),
                POV_MARRIED_OWN_EST  = sum(POV_MARRIED_OWN_EST , na.rm=TRUE),
                POV_MARRIED_RENT_EST  = sum(POV_MARRIED_RENT_EST , na.rm=TRUE),
                POV_MARRIED_TOT_MOE  = sum(POV_MARRIED_TOT_MOE , na.rm=TRUE),
                POV_MARRIED_OWN_MOE  = sum(POV_MARRIED_OWN_MOE , na.rm=TRUE),
                POV_MARRIED_RENT_MOE  = sum(POV_MARRIED_RENT_MOE , na.rm=TRUE),
                POV_MALE_TOT_EST  = sum(POV_MALE_TOT_EST , na.rm=TRUE),
                POV_MALE_OWN_EST  = sum(POV_MALE_OWN_EST , na.rm=TRUE),
                POV_MALE_RENT_EST  = sum(POV_MALE_RENT_EST , na.rm=TRUE),
                POV_MALE_TOT_MOE  = sum(POV_MALE_TOT_MOE , na.rm=TRUE),
                POV_MALE_OWN_MOE  = sum(POV_MALE_OWN_MOE , na.rm=TRUE),
                POV_MALE_RENT_MOE  = sum(POV_MALE_RENT_MOE , na.rm=TRUE),
                POV_FEMALE_TOT_EST  = sum(POV_FEMALE_TOT_EST , na.rm=TRUE),
                POV_FEMALE_OWN_EST  = sum(POV_FEMALE_OWN_EST , na.rm=TRUE),
                POV_FEMALE_RENT_EST  = sum(POV_FEMALE_RENT_EST , na.rm=TRUE),
                POV_FEMALE_TOT_MOE  = sum(POV_FEMALE_TOT_MOE , na.rm=TRUE),
                POV_FEMALE_OWN_MOE  = sum(POV_FEMALE_OWN_MOE , na.rm=TRUE),
                POV_FEMALE_RENT_MOE  = sum(POV_FEMALE_RENT_MOE , na.rm=TRUE),
                
                TOT_TOT_OWN_EST =  sum(TOT_TOT_OWN_EST, na.rm=TRUE),
                TOT_TOT_OWN_MOE =  sum(TOT_TOT_OWN_MOE, na.rm=TRUE),
                TOT_TOT_RENT_EST =  sum(TOT_TOT_RENT_EST, na.rm=TRUE),
                TOT_TOT_RENT_MOE =  sum(TOT_TOT_RENT_MOE, na.rm=TRUE),
                TOT_MARRIED_TOT_EST  = sum(TOT_MARRIED_TOT_EST , na.rm=TRUE),
                TOT_MARRIED_OWN_EST  = sum(TOT_MARRIED_OWN_EST , na.rm=TRUE),
                TOT_MARRIED_RENT_EST  = sum(TOT_MARRIED_RENT_EST , na.rm=TRUE),
                TOT_MARRIED_TOT_MOE  = sum(TOT_MARRIED_TOT_MOE , na.rm=TRUE),
                TOT_MARRIED_OWN_MOE  = sum(TOT_MARRIED_OWN_MOE , na.rm=TRUE),
                TOT_MARRIED_RENT_MOE  = sum(TOT_MARRIED_RENT_MOE , na.rm=TRUE),
                TOT_MALE_TOT_EST  = sum(TOT_MALE_TOT_EST , na.rm=TRUE),
                TOT_MALE_OWN_EST  = sum(TOT_MALE_OWN_EST , na.rm=TRUE),
                TOT_MALE_RENT_EST  = sum(TOT_MALE_RENT_EST , na.rm=TRUE),
                TOT_MALE_TOT_MOE  = sum(TOT_MALE_TOT_MOE , na.rm=TRUE),
                TOT_MALE_OWN_MOE  = sum(TOT_MALE_OWN_MOE , na.rm=TRUE),
                TOT_MALE_RENT_MOE  = sum(TOT_MALE_RENT_MOE , na.rm=TRUE),
                TOT_FEMALE_TOT_EST  = sum(TOT_FEMALE_TOT_EST , na.rm=TRUE),
                TOT_FEMALE_OWN_EST  = sum(TOT_FEMALE_OWN_EST , na.rm=TRUE),
                TOT_FEMALE_RENT_EST  = sum(TOT_FEMALE_RENT_EST , na.rm=TRUE),
                TOT_FEMALE_TOT_MOE  = sum(TOT_FEMALE_TOT_MOE , na.rm=TRUE),
                TOT_FEMALE_OWN_MOE  = sum(TOT_FEMALE_OWN_MOE , na.rm=TRUE),
                TOT_FEMALE_RENT_MOE  = sum(TOT_FEMALE_RENT_MOE , na.rm=TRUE)
      ) %>%
      mutate(GEOID = "08000.5",
             NAME = name1) %>%
      select(GEOID, NAME, TOT_TOT_TOT_EST : TOT_FEMALE_RENT_MOE)
    
    f.B17019_US <- f.B17019_ACS %>% filter(GEOID == "01000")
    f.B17019_CO <- f.B17019_ACS %>% filter(GEOID == "08000")
    f.B17019_CTY <- f.B17019_ACS %>% filter(GEOID %in% cty_str)
    
    f.B17019_FIN <- bind_rows(f.B17019_US, f.B17019_CO, f.B17019_csbg, f.B17019_CTY)
  } else {
    f.B17019_FIN <- f.B17019_ACS %>% filter(GEOID %in% full_str)
  }
  
  # calculating Percentage
  f.B17019_FIN <- f.B17019_FIN %>%
    mutate(NAME = str_replace(NAME, ", Colorado",""),
           TOT_TOT_TOT_MOE = sqrt(TOT_TOT_TOT_MOE),
           POV_TOT_TOT_MOE = sqrt(POV_TOT_TOT_MOE),
           POV_TOT_OWN_MOE = sqrt(POV_TOT_OWN_MOE),
           POV_TOT_RENT_MOE = sqrt(POV_TOT_RENT_MOE),
           POV_MARRIED_TOT_MOE = sqrt(POV_MARRIED_TOT_MOE),
           POV_MARRIED_OWN_MOE = sqrt(POV_MARRIED_OWN_MOE),
           POV_MARRIED_RENT_MOE = sqrt(POV_MARRIED_RENT_MOE),
           POV_MALE_TOT_MOE = sqrt(POV_MALE_TOT_MOE),
           POV_MALE_OWN_MOE = sqrt(POV_MALE_OWN_MOE),
           POV_MALE_RENT_MOE = sqrt(POV_MALE_RENT_MOE),
           POV_FEMALE_TOT_MOE = sqrt(POV_FEMALE_TOT_MOE),
           POV_FEMALE_OWN_MOE = sqrt(POV_FEMALE_OWN_MOE),
           POV_FEMALE_RENT_MOE = sqrt(POV_FEMALE_RENT_MOE),
           TOT_TOT_OWN_MOE = sqrt(TOT_TOT_OWN_MOE),
           TOT_TOT_RENT_MOE = sqrt(TOT_TOT_RENT_MOE),
           TOT_MARRIED_TOT_MOE = sqrt(TOT_MARRIED_TOT_MOE),
           TOT_MARRIED_OWN_MOE = sqrt(TOT_MARRIED_OWN_MOE),
           TOT_MARRIED_RENT_MOE = sqrt(TOT_MARRIED_RENT_MOE),
           TOT_MALE_TOT_MOE = sqrt(TOT_MALE_TOT_MOE),
           TOT_MALE_OWN_MOE = sqrt(TOT_MALE_OWN_MOE),
           TOT_MALE_RENT_MOE = sqrt(TOT_MALE_RENT_MOE),
           TOT_FEMALE_TOT_MOE = sqrt(TOT_FEMALE_TOT_MOE),
           TOT_FEMALE_OWN_MOE = sqrt(TOT_FEMALE_OWN_MOE),
           TOT_FEMALE_RENT_MOE = sqrt(TOT_FEMALE_RENT_MOE),
           
           POV_TOT_OWN_EST_PCT = POV_TOT_OWN_EST/POV_TOT_TOT_EST,
           POV_TOT_OWN_MOE_PCT = pctMOE(POV_TOT_TOT_EST,POV_TOT_TOT_MOE,POV_TOT_OWN_MOE,POV_TOT_OWN_EST_PCT),
           POV_TOT_RENT_EST_PCT = POV_TOT_RENT_EST/POV_TOT_TOT_EST,
           POV_TOT_RENT_MOE_PCT = pctMOE(POV_TOT_TOT_EST,POV_TOT_TOT_MOE,POV_TOT_RENT_MOE,POV_TOT_RENT_EST_PCT),
           POV_MARRIED_OWN_EST_PCT = POV_MARRIED_OWN_EST/POV_MARRIED_TOT_EST,
           POV_MARRIED_OWN_MOE_PCT = pctMOE(POV_MARRIED_TOT_EST,POV_MARRIED_TOT_MOE,POV_MARRIED_OWN_MOE,POV_MARRIED_OWN_EST_PCT),
           POV_MARRIED_RENT_EST_PCT = POV_MARRIED_RENT_EST/POV_MARRIED_TOT_EST,
           POV_MARRIED_RENT_MOE_PCT = pctMOE(POV_MARRIED_TOT_EST,POV_MARRIED_TOT_MOE,POV_MARRIED_RENT_MOE,POV_MARRIED_RENT_EST_PCT),
           POV_MALE_OWN_EST_PCT = POV_MALE_OWN_EST/POV_MALE_TOT_EST,
           POV_MALE_OWN_MOE_PCT = pctMOE(POV_MALE_TOT_EST,POV_MALE_TOT_MOE,POV_MALE_OWN_MOE,POV_MALE_OWN_EST_PCT),
           POV_MALE_RENT_EST_PCT = POV_MALE_RENT_EST/POV_MALE_TOT_EST,
           POV_MALE_RENT_MOE_PCT = pctMOE(POV_MALE_TOT_EST,POV_MALE_TOT_MOE,POV_MALE_RENT_MOE,POV_MALE_RENT_EST_PCT),
           POV_FEMALE_OWN_EST_PCT = POV_FEMALE_OWN_EST/POV_FEMALE_TOT_EST,
           POV_FEMALE_OWN_MOE_PCT = pctMOE(POV_FEMALE_TOT_EST,POV_FEMALE_TOT_MOE,POV_FEMALE_OWN_MOE,POV_FEMALE_OWN_EST_PCT),
           POV_FEMALE_RENT_EST_PCT = POV_FEMALE_RENT_EST/POV_FEMALE_TOT_EST,
           POV_FEMALE_RENT_MOE_PCT = pctMOE(POV_FEMALE_TOT_EST,POV_FEMALE_TOT_MOE,POV_FEMALE_RENT_MOE,POV_FEMALE_RENT_EST_PCT),

           TOT_TOT_OWN_EST_PCT = TOT_TOT_OWN_EST/TOT_TOT_TOT_EST,
           TOT_TOT_OWN_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,TOT_TOT_OWN_MOE,TOT_TOT_OWN_EST_PCT),
           TOT_TOT_RENT_EST_PCT = TOT_TOT_RENT_EST/TOT_TOT_TOT_EST,
           TOT_TOT_RENT_MOE_PCT = pctMOE(TOT_TOT_TOT_EST,TOT_TOT_TOT_MOE,TOT_TOT_RENT_MOE,TOT_TOT_RENT_EST_PCT),
           TOT_MARRIED_OWN_EST_PCT = TOT_MARRIED_OWN_EST/TOT_MARRIED_TOT_EST,
           TOT_MARRIED_OWN_MOE_PCT = pctMOE(TOT_MARRIED_TOT_EST,TOT_MARRIED_TOT_MOE,TOT_MARRIED_OWN_MOE,TOT_MARRIED_OWN_EST_PCT),
           TOT_MARRIED_RENT_EST_PCT = TOT_MARRIED_RENT_EST/TOT_MARRIED_TOT_EST,
           TOT_MARRIED_RENT_MOE_PCT = pctMOE(TOT_MARRIED_TOT_EST,TOT_MARRIED_TOT_MOE,TOT_MARRIED_RENT_MOE,TOT_MARRIED_RENT_EST_PCT),
           
           
           TOT_MALE_OWN_EST_PCT = TOT_MALE_OWN_EST/TOT_MALE_TOT_EST,
           TOT_MALE_OWN_MOE_PCT = pctMOE(TOT_MALE_TOT_EST,TOT_MALE_TOT_MOE,TOT_MALE_OWN_MOE,TOT_MALE_OWN_EST_PCT),
           TOT_MALE_RENT_EST_PCT = TOT_MALE_RENT_EST/TOT_MALE_TOT_EST,
           TOT_MALE_RENT_MOE_PCT = pctMOE(TOT_MALE_TOT_EST,TOT_MALE_TOT_MOE,TOT_MALE_RENT_MOE,TOT_MALE_RENT_EST_PCT),
           TOT_FEMALE_OWN_EST_PCT = TOT_FEMALE_OWN_EST/TOT_FEMALE_TOT_EST,
           TOT_FEMALE_OWN_MOE_PCT = pctMOE(TOT_FEMALE_TOT_EST,TOT_FEMALE_TOT_MOE,TOT_FEMALE_OWN_MOE,TOT_FEMALE_OWN_EST_PCT),
           TOT_FEMALE_RENT_EST_PCT = TOT_FEMALE_RENT_EST/TOT_FEMALE_TOT_EST,
           TOT_FEMALE_RENT_MOE_PCT = pctMOE(TOT_FEMALE_TOT_EST,TOT_FEMALE_TOT_MOE,TOT_FEMALE_RENT_MOE,TOT_FEMALE_RENT_EST_PCT)
           )  %>% 
    select(GEOID, NAME, TOT_TOT_TOT_EST : TOT_FEMALE_RENT_MOE_PCT)


  # Building Chart data sets
  f.hh_est <- f.B17019_FIN %>% select(GEOID, NAME, 
                                       POV_FEMALE_OWN_EST_PCT,	POV_FEMALE_RENT_EST_PCT,	
                                       POV_MALE_OWN_EST_PCT,	POV_MALE_RENT_EST_PCT,	
                                       POV_MARRIED_OWN_EST_PCT,	POV_MARRIED_RENT_EST_PCT,	
                                       POV_TOT_OWN_EST_PCT,	POV_TOT_RENT_EST_PCT,	
                                       TOT_FEMALE_OWN_EST_PCT,	TOT_FEMALE_RENT_EST_PCT,	
                                       TOT_MALE_OWN_EST_PCT,	TOT_MALE_RENT_EST_PCT,	
                                       TOT_MARRIED_OWN_EST_PCT,	TOT_MARRIED_RENT_EST_PCT,	
                                       TOT_TOT_OWN_EST_PCT,	TOT_TOT_RENT_EST_PCT) %>%
    gather(FAM_TYPE, EST_PCT, POV_FEMALE_OWN_EST_PCT : TOT_TOT_RENT_EST_PCT, factor_key =TRUE) %>%
    mutate(FAM_TYPE = str_replace(FAM_TYPE,"_EST_PCT",""))
  
  f.hh_moe <- f.B17019_FIN %>% select(GEOID, NAME, 
                                       POV_FEMALE_OWN_MOE_PCT,	POV_FEMALE_RENT_MOE_PCT,	
                                       POV_MALE_OWN_MOE_PCT,	POV_MALE_RENT_MOE_PCT,	
                                       POV_MARRIED_OWN_MOE_PCT,	POV_MARRIED_RENT_MOE_PCT,	
                                       POV_TOT_OWN_MOE_PCT,	POV_TOT_RENT_MOE_PCT,	
                                       TOT_FEMALE_OWN_MOE_PCT,	TOT_FEMALE_RENT_MOE_PCT,	
                                       TOT_MALE_OWN_MOE_PCT,	TOT_MALE_RENT_MOE_PCT,	
                                       TOT_MARRIED_OWN_MOE_PCT,	TOT_MARRIED_RENT_MOE_PCT,	
                                       TOT_TOT_OWN_MOE_PCT,	TOT_TOT_RENT_MOE_PCT) %>%
    gather(FAM_TYPE, MOE_PCT, POV_FEMALE_OWN_MOE_PCT : TOT_TOT_RENT_MOE_PCT, factor_key =TRUE) %>%
    mutate(FAM_TYPE = str_replace(FAM_TYPE,"_MOE_PCT","")) %>% select(-NAME)
  
  
  f.hh_chart <- inner_join(f.hh_est, f.hh_moe, by =c('GEOID',"FAM_TYPE"))   %>%
    mutate(EST_PCT = EST_PCT * 100,
           MOE_PCT = MOE_PCT * 100,
           TYPE = ifelse(str_sub(FAM_TYPE,1,3) == "POV",0,1),
           TENURE = ifelse(grepl("OWN",FAM_TYPE),1,0),
           FAM_TYPE = str_sub(FAM_TYPE,5,nchar(FAM_TYPE)),
           FAM_TYPE = str_replace(FAM_TYPE,"_RENT",""),
           FAM_TYPE = str_replace(FAM_TYPE,"_OWN","")) %>% replace(is.na(.),0)
  f.hh_chart_tab <- f.hh_chart
  
  
  f.hh_chart$FAM_TYPE <- factor(f.hh_chart$FAM_TYPE, 
                                 levels = c("FEMALE",  "MALE", "MARRIED", "TOT"),
                                 labels = c("Female Householder\nNo Spouse Present",
                                            "Male Householder\nNo Spouse Present",
                                            "Married Couple Families",
                                            "All Families"))
  
  f.hh_chart$TYPE <- factor(f.hh_chart$TYPE, levels=c(0,1),
                             labels =c("Below Federal Poverty Level",
                                       "Total Population"))
  f.hh_chart$TENURE <- factor(f.hh_chart$TENURE, levels=c(0,1),
                                 labels =c("Rent",
                                           "Own"))
  
  nameList <- unique(f.hh_chart$NAME)
  f.hh_chart$NAME <- factor(f.hh_chart$NAME, levels =nameList)
  
  chart_list <- list()
  pltTitle <- paste0("Housing Tenure by Family Type and Poverty Status: ", name1)
  
  if(length(cty_str) > 1) {
    f.hh_chart3 <- f.hh_chart %>% filter(!(GEOID %in% cty_str))
  } else {
    f.hh_chart3 <- f.hh_chart
  }
  barCol6 <- c("#359A7E", "#7FFF00", "#6D3A5D", "#F08080", "#007ADE", "#ADD8E6")
  barCol4 <- c("#007ADE", "#ADD8E6", "#C0504D", "#F08080")
  

  
  # Creating Bar Charts
  ggimg3 <-ggplot(f.hh_chart3, aes(x =as.factor(FAM_TYPE), y =EST_PCT, fill = interaction(TENURE, NAME))) +
    geom_bar(stat ="identity", position = "dodge", color ="black") + 
    geom_text(aes(x =as.factor(FAM_TYPE), y =EST_PCT, label =percent(EST_PCT), group = interaction(TENURE, NAME)), 
              position = position_dodge(width =1.0),
              vjust =-0.8,  size = 2) +
    scale_fill_manual(values =barCol6) +
    scale_y_continuous(breaks = seq(0, 100, by=20), limits = c(0, 120), label =percent,  expand = c(0, 0)) +
    facet_wrap(vars(TYPE), ncol=1) +
    labs(title = pltTitle,
         subtitle = "CSBG Comparison",
         caption = outCap,
         x = "Family Type",
         y = "Percentage",
         fill ="Housing Tenure, CSBG Entity",
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
    f.chart_data <- f.hh_chart %>% filter(GEOID == "08000.5" | GEOID == cty_str[i]) 
    
    ggimg3 <-ggplot(  f.chart_data, aes(x =as.factor(FAM_TYPE), y =EST_PCT, fill = interaction(TENURE, NAME))) +
      geom_bar(stat ="identity", position = "dodge", color ="black") + 
      geom_text(aes(x =as.factor(FAM_TYPE), y =EST_PCT, label =percent(EST_PCT), group = interaction(TENURE, NAME)), 
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
           fill ="Housing Tenure, CSBG Entity",
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
  
  f.B17019_FIN$NAME <- factor(f.B17019_FIN$NAME ,levels=name_list)
  
  f.hh_pov <- f.B17019_FIN %>% 
    mutate(TYPE = 0) %>%
    select(GEOID, NAME, TYPE, 
           POV_FEMALE_OWN_EST,	POV_FEMALE_OWN_MOE,	POV_FEMALE_OWN_EST_PCT, POV_FEMALE_OWN_MOE_PCT,	
           POV_FEMALE_RENT_EST,	POV_FEMALE_RENT_MOE,	POV_FEMALE_RENT_EST_PCT, POV_FEMALE_RENT_MOE_PCT,
           POV_FEMALE_TOT_EST,	POV_FEMALE_TOT_MOE,	
           POV_MALE_OWN_EST,	POV_MALE_OWN_MOE, POV_MALE_OWN_EST_PCT,	POV_MALE_OWN_MOE_PCT,	
           POV_MALE_RENT_EST,	POV_MALE_RENT_MOE, POV_MALE_RENT_EST_PCT,	POV_MALE_RENT_MOE_PCT,	
           POV_MALE_TOT_EST,	POV_MALE_TOT_MOE,	
           POV_MARRIED_OWN_EST,	POV_MARRIED_OWN_MOE,	POV_MARRIED_OWN_EST_PCT,	POV_MARRIED_OWN_MOE_PCT,	
           POV_MARRIED_RENT_EST,	POV_MARRIED_RENT_MOE,	POV_MARRIED_RENT_EST_PCT,	POV_MARRIED_RENT_MOE_PCT,	
           POV_MARRIED_TOT_EST,	POV_MARRIED_TOT_MOE,	
           POV_TOT_OWN_EST,	POV_TOT_OWN_MOE,	POV_TOT_OWN_EST_PCT,	POV_TOT_OWN_MOE_PCT,	
           POV_TOT_RENT_EST,	POV_TOT_RENT_MOE,	POV_TOT_RENT_EST_PCT,	POV_TOT_RENT_MOE_PCT,	
           POV_TOT_TOT_EST,	POV_TOT_TOT_MOE)
  names(f.hh_pov) <- c("GEOID", "NAME",	"TYPE",	
                        "FEMALE_OWN_EST",	"FEMALE_OWN_MOE",	"FEMALE_OWN_EST_PCT", "FEMALE_OWN_MOE_PCT",	
                        "FEMALE_RENT_EST",	"FEMALE_RENT_MOE",	"FEMALE_RENT_EST_PCT", "FEMALE_RENT_MOE_PCT",	
                        "FEMALE_TOT_EST",	"FEMALE_TOT_MOE",	
                        "MALE_OWN_EST",	"MALE_OWN_MOE", "MALE_OWN_EST_PCT",	"MALE_OWN_MOE_PCT",	
                        "MALE_RENT_EST",	"MALE_RENT_MOE", "MALE_RENT_EST_PCT",	"MALE_RENT_MOE_PCT",	
                        "MALE_TOT_EST",	"MALE_TOT_MOE",	
                        "MARRIED_OWN_EST",	"MARRIED_OWN_MOE",	"MARRIED_OWN_EST_PCT",	"MARRIED_OWN_MOE_PCT",	
                        "MARRIED_RENT_EST",	"MARRIED_RENT_MOE",	"MARRIED_RENT_EST_PCT",	"MARRIED_RENT_MOE_PCT",	
                        "MARRIED_TOT_EST",	"MARRIED_TOT_MOE",	
                        "TOT_OWN_EST",	"TOT_OWN_MOE",	"TOT_OWN_EST_PCT",	"TOT_OWN_MOE_PCT",	
                        "TOT_RENT_EST",	"TOT_RENT_MOE",	"TOT_RENT_EST_PCT",	"TOT_RENT_MOE_PCT",
                        "TOT_TOT_EST",	"TOT_TOT_MOE")
  
  
  f.hh_tot <- f.B17019_FIN %>% 
    mutate(TYPE = 1,
           NAME = "") %>%
    select(GEOID, NAME, TYPE, 
           TOT_FEMALE_OWN_EST,	TOT_FEMALE_OWN_MOE,	TOT_FEMALE_OWN_EST_PCT, TOT_FEMALE_OWN_MOE_PCT,	
           TOT_FEMALE_RENT_EST,	TOT_FEMALE_RENT_MOE,	TOT_FEMALE_RENT_EST_PCT, TOT_FEMALE_RENT_MOE_PCT,
           TOT_FEMALE_TOT_EST,	TOT_FEMALE_TOT_MOE,	
           TOT_MALE_OWN_EST,	TOT_MALE_OWN_MOE, TOT_MALE_OWN_EST_PCT,	TOT_MALE_OWN_MOE_PCT,	
           TOT_MALE_RENT_EST,	TOT_MALE_RENT_MOE, TOT_MALE_RENT_EST_PCT,	TOT_MALE_RENT_MOE_PCT,	
           TOT_MALE_TOT_EST,	TOT_MALE_TOT_MOE,	
           TOT_MARRIED_OWN_EST,	TOT_MARRIED_OWN_MOE,	TOT_MARRIED_OWN_EST_PCT,	TOT_MARRIED_OWN_MOE_PCT,	
           TOT_MARRIED_RENT_EST,	TOT_MARRIED_RENT_MOE,	TOT_MARRIED_RENT_EST_PCT,	TOT_MARRIED_RENT_MOE_PCT,	
           TOT_MARRIED_TOT_EST,	TOT_MARRIED_TOT_MOE,	
           TOT_TOT_OWN_EST,	TOT_TOT_OWN_MOE,	TOT_TOT_OWN_EST_PCT,	TOT_TOT_OWN_MOE_PCT,	
           TOT_TOT_RENT_EST,	TOT_TOT_RENT_MOE,	TOT_TOT_RENT_EST_PCT,	TOT_TOT_RENT_MOE_PCT,	
           TOT_TOT_TOT_EST,	TOT_TOT_TOT_MOE)
  names(f.hh_tot) <- c("GEOID", "NAME",	"TYPE",	
                       "FEMALE_OWN_EST",	"FEMALE_OWN_MOE",	"FEMALE_OWN_EST_PCT", "FEMALE_OWN_MOE_PCT",	
                       "FEMALE_RENT_EST",	"FEMALE_RENT_MOE",	"FEMALE_RENT_EST_PCT", "FEMALE_RENT_MOE_PCT",	
                       "FEMALE_TOT_EST",	"FEMALE_TOT_MOE",	
                       "MALE_OWN_EST",	"MALE_OWN_MOE", "MALE_OWN_EST_PCT",	"MALE_OWN_MOE_PCT",	
                       "MALE_RENT_EST",	"MALE_RENT_MOE", "MALE_RENT_EST_PCT",	"MALE_RENT_MOE_PCT",	
                       "MALE_TOT_EST",	"MALE_TOT_MOE",	
                       "MARRIED_OWN_EST",	"MARRIED_OWN_MOE",	"MARRIED_OWN_EST_PCT",	"MARRIED_OWN_MOE_PCT",	
                       "MARRIED_RENT_EST",	"MARRIED_RENT_MOE",	"MARRIED_RENT_EST_PCT",	"MARRIED_RENT_MOE_PCT",	
                       "MARRIED_TOT_EST",	"MARRIED_TOT_MOE",	
                       "TOT_OWN_EST",	"TOT_OWN_MOE",	"TOT_OWN_EST_PCT",	"TOT_OWN_MOE_PCT",	
                       "TOT_RENT_EST",	"TOT_RENT_MOE",	"TOT_RENT_EST_PCT",	"TOT_RENT_MOE_PCT",
                       "TOT_TOT_EST",	"TOT_TOT_MOE")
  
  
  f.hh_tab_fin_l <- bind_rows(f.hh_pov, f.hh_tot)  %>%
    arrange(GEOID, TYPE) %>%
    select(-GEOID) 
  

  
  f.hh_tab_fin_l[,c(3,4,7,8,11,12,13,14,17,18,21,22,23,24,27,28,31,32,33,34,37,38,41,42)] <-
    sapply(f.hh_tab_fin_l[,c(3,4,7,8,11,12,13,14,17,18,21,22,23,24,27,28,31,32,33,34,37,38,41,42)], 
           function(x) comma_conv(x))
  f.hh_tab_fin_l[,c(5,6,9,10,15,16,19,20,25,26,29,30,35,36,39,40)] <-
    sapply(f.hh_tab_fin_l[,c(5,6,9,10,15,16,19,20,25,26,29,30,35,36,39,40)], function(x) percent(x * 100))
  

  f.hh_tab_fin_l$TYPE <- factor(f.hh_tab_fin_l$TYPE, levels=c(0,1),
                                 labels =c("Families Below Federal Poverty Level",
                                           "All Families"))
  
  # Flex Table
  tab_head <- paste0("Housing Tenure by Family Type by Poverty Status, ",listID$plName1)
  head_level1 <- c("",	"",	"Female Householder, No Spouse Present",	"",	"",	"",	"",	"",	"",	"",	"",	"",	
                   "Male Householder, No Spouse Present",	"",	"",	"",	"",	"",	"",	"",	"",	"",	
                   "Married Couple Families",	"",	"",	"",	"",	"",	"",	"",	"",	"",	
                   "All Families",	"",	"",	"",	"",	"",	"",	"",	"",	"")
  head_level2 <- c("",	"",	"Owner Occupied Homes",	"",	"",	"",	"Rental Homes",	"",	"",	"",	"All Families",	"",	
                   "Owner Occupied Homes",	"",	"",	"",	"Rental Homes",	"",	"",	"",	"All Families",	"",	
                   "Owner Occupied Homes",	"",	"",	"",	"Rental Homes",	"",	"",	"",	"All Families",	"",	
                   "Owner Occupied Homes",	"",	"",	"",	"Rental Homes",	"",	"",	"",	"All Families",	"")
  head_measure <- c("CSBG Entity","Poverty Status",rep(c("Estimate","Margin of Error"),20))
  
  headers <-t(bind_cols(as.data.frame(head_level1), as.data.frame(head_level2), as.data.frame(head_measure)))
  
  f.hhFlex <- flextable(
    f.hh_tab_fin_l,
    col_keys = names(f.hh_tab_fin_l)) %>%
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

  f.hhFlex <- add_header_lines(f.hhFlex,values =tab_head, top =TRUE)
  
  
  
  #bind list
  outList <- list("plot" = chart_list, "data" =  f.hh_tab_fin_l, "header_sh" = headers, "FlexTable" = f.hhFlex)
  
  return(outList)
} # funciton end
