#'ageEmployment Outputs Table of Age by employment stats and charts
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
ageEmployment <- function(lvl,listID, ACS,curYr) {

  # Collecting place ids from  idList, setting default values
   outCap <- captionSrc("ACS",ACS,"B23001") 
    ctyfips <- as.character(as.numeric(substr(listID$list1,3,5)))
   
   f.ctyEMP <- codemog_api(data="b23001",db=ACS,sumlev="50",geography="sumlev",meta="no")
   f.ctyEMP[,c(3,8:180)] <- sapply(f.ctyEMP[,c(3,8:180)],as.numeric)

   f.ctyEMP_cty <- f.ctyEMP %>%
             filter(county %in% ctyfips) %>%
            group_by(county) %>%
           mutate(
            TOT.1619.M = 	b23001003,
            LF.1619.M =	b23001004,
            CIVIL.1619.M = 	b23001006,
            EMP.1619.M =	b23001007,
            UNEMP.1619.M = 	b23001008,
            NLF.1619.M = 	b23001009,
            
            TOT.2064.M = 	b23001010 + b23001017 + b23001024 + b23001031 + b23001038 + b23001045 + b23001052 + b23001059 + b23001066,
            LF.2064.M =	b23001011 + b23001018 + b23001025 + b23001032 + b23001039 + b23001046 + b23001053 + b23001060 + b23001067,
            CIVIL.2064.M = 	b23001013 + b23001020 + b23001027 + b23001034 + b23001041 + b23001048 + b23001055 + b23001062 + b23001069,
            EMP.2064.M =	b23001014 + b23001021 + b23001028 + b23001035 + b23001042 + b23001049 + b23001056 + b23001063 + b23001070,
            UNEMP.2064.M = 	b23001015 + b23001022 + b23001029 + b23001036 + b23001043 + b23001050 + b23001057 + b23001064 + b23001071,
            NLF.2064.M = 	b23001016 + b23001023 + b23001030 + b23001037 + b23001044 + b23001051 + b23001058 + b23001065 + b23001072,
            	
            TOT.65.M =	b23001073+ b23001078 + b23001083,
            LF.65.M = 	b23001074+ b23001079 + b23001084,
            EMP.65.M = 	b23001075+ b23001080 + b23001085,
            UNEMP.65.M = 	b23001076+ b23001081 + b23001086,
            NLF.65.M = 	b23001077+ b23001082 + b23001087,
            	
            TOT.1619.F = 	b23001089,
            LF.1619.F =	b23001090,
            CIVIL.1619.F = 	b23001092,
            EMP.1619.F =	b23001093,
            UNEMP.1619.F = 	b23001094,
            NLF.1619.F = 	b23001095,
            
            
            TOT.2064.F = 	b23001096 + b23001103 + b23001110 + b23001117 + b23001124 + b23001131 + b23001138 + b23001145 + b23001152,
            LF.2064.F =	b23001097 + b23001104 + b23001111 + b23001118 + b23001125 + b23001132 + b23001139 + b23001146 + b23001153,
            CIVIL.2064.F = 	b23001099 + b23001106 + b23001113 + b23001120 + b23001127 + b23001134 + b23001141 + b23001148 + b23001155,
            EMP.2064.F =	b23001100 + b23001107 + b23001114 + b23001121 + b23001128 + b23001135 + b23001142 + b23001149 + b23001156,
            UNEMP.2064.F = 	b23001101 + b23001108 + b23001115 + b23001122 + b23001129 + b23001136 + b23001143 + b23001150 + b23001157,
            NLF.2064.F = 	b23001102 + b23001109 + b23001116 + b23001123 + b23001130 + b23001137 + b23001144 + b23001151 + b23001158,
            
            TOT.65.F =	b23001159+ b23001164 + b23001169,
            LF.65.F = 	b23001160+ b23001165 + b23001170,
            EMP.65.F = 	b23001161+ b23001166 + b23001171,
            UNEMP.65.F = 	b23001162+ b23001167 + b23001172,
            NLF.65.F = 	b23001163+ b23001168 + b23001173,
            
            LF.1619	 = LF.1619.M + LF.1619.F,
            NLF.1619	 = NLF.1619.M + NLF.1619.F,
            TOT.1619 = TOT.1619.M + TOT.1619.F,
            EMP.1619	 = EMP.1619.M + EMP.1619.F,
            UNEMP.1619	 = UNEMP.1619.M + UNEMP.1619.F,
            CIVIL.1619	 = CIVIL.1619.M + CIVIL.1619.F,
            
            LF.2064	 = LF.2064.M + LF.2064.F,
            NLF.2064	 = NLF.2064.M + NLF.2064.F,
            TOT.2064 = TOT.2064.M + TOT.2064.F,
            EMP.2064	 = EMP.2064.M + EMP.2064.F,
            UNEMP.2064	 = UNEMP.2064.M + UNEMP.2064.F,
            CIVIL.2064	 = CIVIL.2064.M + CIVIL.2064.F,
            
            LF.65	 = LF.65.M + LF.65.F,
            NLF.65	 = NLF.65.M + NLF.65.F,
            TOT.65 = TOT.65.M + TOT.65.F,
            EMP.65	 = EMP.65.M + EMP.65.F,
            UNEMP.65	 = UNEMP.65.M + UNEMP.65.F,
            CIVIL.65	 = LF.65,
            
            LF.1619.P	 = LF.1619/TOT.1619,
            NLF.1619.P	 = NLF.1619/TOT.1619,
            TOT.1619.P = TOT.1619/TOT.1619,
            EMP.1619.P	 = EMP.1619/CIVIL.1619,
            UNEMP.1619.P	 = UNEMP.1619/CIVIL.1619,
            CIVIL.1619.P	 = CIVIL.1619/TOT.1619,

            LF.2064.P	 = LF.2064/TOT.2064,
            NLF.2064.P	 = NLF.2064/TOT.2064,
            TOT.2064.P = TOT.2064/TOT.2064,
            EMP.2064.P	 = EMP.2064/CIVIL.2064,
            UNEMP.2064.P	 = UNEMP.2064/CIVIL.2064,
            CIVIL.2064.P	 = CIVIL.2064/TOT.2064,
            
            LF.65.P	 = LF.65/TOT.65,
            NLF.65.P	 = NLF.65/TOT.65,
            TOT.65.P = TOT.65/TOT.65,
            EMP.65.P	 = EMP.65/CIVIL.65,
            UNEMP.65.P	 = UNEMP.65/CIVIL.65,
            CIVIL.65.P	 = CIVIL.65/TOT.65
 
 )
 
   f.ctyEMP_cty <- f.ctyEMP_cty[,c("geoname",	"county",				
                      "LF.1619", "NLF.1619", "TOT.1619", "EMP.1619", "UNEMP.1619", "CIVIL.1619",
                     "LF.2064", "NLF.2064", "TOT.2064", "EMP.2064", "UNEMP.2064", "CIVIL.2064",
                      "LF.65", "NLF.65", "TOT.65", "EMP.65", "UNEMP.65", "CIVIL.65",
                      "LF.1619.P", "NLF.1619.P", "TOT.1619.P", "EMP.1619.P", "UNEMP.1619.P", "CIVIL.1619.P",
                      "LF.2064.P", "NLF.2064.P", "TOT.2064.P", "EMP.2064.P", "UNEMP.2064.P", "CIVIL.2064.P",
                      "LF.65.P", "NLF.65.P", "TOT.65.P", "EMP.65.P", "UNEMP.65.P", "CIVIL.65.P")]
 
if(length(ctyfips) > 1) {
     f.ctyEMP_agy <- f.ctyEMP %>%
             filter(county %in% ctyfips) %>%
          summarize(
                  TOT.1619.M = sum(b23001003),
                  LF.1619.M = sum(b23001004),
                  CIVIL.1619.M = sum(b23001006),
                  EMP.1619.M = sum(b23001007),
                  UNEMP.1619.M = sum(b23001008),
                  NLF.1619.M = sum(b23001009),

                  TOT.2064.M = sum(b23001010, b23001017, b23001024, b23001031, b23001038, b23001045, b23001052, b23001059, b23001066),
                  LF.2064.M = sum(b23001011, b23001018, b23001025, b23001032, b23001039, b23001046, b23001053, b23001060, b23001067),
                  CIVIL.2064.M = sum(b23001013, b23001020, b23001027, b23001034, b23001041, b23001048, b23001055, b23001062, b23001069),
                  EMP.2064.M = sum(b23001014, b23001021, b23001028, b23001035, b23001042, b23001049, b23001056, b23001063, b23001070),
                  UNEMP.2064.M = sum(b23001015, b23001022, b23001029, b23001036, b23001043, b23001050, b23001057, b23001064, b23001071),
                  NLF.2064.M = sum(b23001016, b23001023, b23001030, b23001037, b23001044, b23001051, b23001058, b23001065, b23001072),
                   
                  TOT.65.M = sum(b23001073+ b23001078, b23001083),
                  LF.65.M = sum(b23001074+ b23001079, b23001084),
                  EMP.65.M = sum(b23001075+ b23001080, b23001085),
                  UNEMP.65.M = sum(b23001076+ b23001081, b23001086),
                  NLF.65.M = sum(b23001077+ b23001082, b23001087),
                   
                  
                  TOT.1619.F = sum(b23001089),
                  LF.1619.F = sum(b23001090),
                  CIVIL.1619.F = sum(b23001092),
                  EMP.1619.F = sum(b23001093),
                  UNEMP.1619.F = sum(b23001094),
                  NLF.1619.F = sum(b23001095),
                  
                  TOT.2064.F = sum(b23001096, b23001103, b23001110, b23001117, b23001124, b23001131, b23001138, b23001145, b23001152),
                  LF.2064.F = sum(b23001097, b23001104, b23001111, b23001118, b23001125, b23001132, b23001139, b23001146, b23001153),
                  CIVIL.2064.F = sum(b23001099, b23001106, b23001113, b23001120, b23001127, b23001134, b23001141, b23001148, b23001155),
                  EMP.2064.F = sum(b23001100, b23001107, b23001114, b23001121, b23001128, b23001135, b23001142, b23001149, b23001156),
                  UNEMP.2064.F = sum(b23001101, b23001108, b23001115, b23001122, b23001129, b23001136, b23001143, b23001150, b23001157),
                  NLF.2064.F = sum(b23001102, b23001109, b23001116, b23001123, b23001130, b23001137, b23001144, b23001151, b23001158),
                   
                  TOT.65.F = sum(b23001159, b23001164, b23001169),
                  LF.65.F = sum(b23001160, b23001165, b23001170),
                  EMP.65.F = sum(b23001161, b23001166, b23001171),
                  UNEMP.65.F = sum(b23001162, b23001167, b23001172),
                  NLF.65.F = sum(b23001163, b23001168, b23001173) ) %>%
           mutate(
            LF.1619	 = LF.1619.M + LF.1619.F,
            NLF.1619	 = NLF.1619.M + NLF.1619.F,
            TOT.1619 = TOT.1619.M + TOT.1619.F,
            EMP.1619	 = EMP.1619.M + EMP.1619.F,
            UNEMP.1619	 = UNEMP.1619.M + UNEMP.1619.F,
            CIVIL.1619	 = CIVIL.1619.M + CIVIL.1619.F,
            
            LF.2064	 = LF.2064.M + LF.2064.F,
            NLF.2064	 = NLF.2064.M + NLF.2064.F,
            TOT.2064 = TOT.2064.M + TOT.2064.F,
            EMP.2064	 = EMP.2064.M + EMP.2064.F,
            UNEMP.2064	 = UNEMP.2064.M + UNEMP.2064.F,
            CIVIL.2064	 = CIVIL.2064.M + CIVIL.2064.F,
            
            LF.65	 = LF.65.M + LF.65.F,
            NLF.65	 = NLF.65.M + NLF.65.F,
            TOT.65 = TOT.65.M + TOT.65.F,
            EMP.65	 = EMP.65.M + EMP.65.F,
            UNEMP.65	 = UNEMP.65.M + UNEMP.65.F,
            CIVIL.65	 = LF.65,
            
            LF.1619.P	 = LF.1619/TOT.1619,
            NLF.1619.P	 = NLF.1619/TOT.1619,
            TOT.1619.P = TOT.1619/TOT.1619,
            EMP.1619.P	 = EMP.1619/CIVIL.1619,
            UNEMP.1619.P	 = UNEMP.1619/CIVIL.1619,
            CIVIL.1619.P	 = CIVIL.1619/TOT.1619,

            LF.2064.P	 = LF.2064/TOT.2064,
            NLF.2064.P	 = NLF.2064/TOT.2064,
            TOT.2064.P = TOT.2064/TOT.2064,
            EMP.2064.P	 = EMP.2064/CIVIL.2064,
            UNEMP.2064.P	 = UNEMP.2064/CIVIL.2064,
            CIVIL.2064.P	 = CIVIL.2064/TOT.2064,
            
            LF.65.P	 = LF.65/TOT.65,
            NLF.65.P	 = NLF.65/TOT.65,
            TOT.65.P = TOT.65/TOT.65,
            EMP.65.P	 = EMP.65/CIVIL.65,
            UNEMP.65.P	 = UNEMP.65/CIVIL.65,
            CIVIL.65.P	 = CIVIL.65/TOT.65)
     
    f.ctyEMP_agy$geoname <- listID$plName1
    f.ctyEMP_agy$county <- 0

   f.ctyEMP_agy <- f.ctyEMP_agy[,c("geoname",	"county",				
                      "LF.1619", "NLF.1619", "TOT.1619", "EMP.1619", "UNEMP.1619", "CIVIL.1619",
                     "LF.2064", "NLF.2064", "TOT.2064", "EMP.2064", "UNEMP.2064", "CIVIL.2064",
                      "LF.65", "NLF.65", "TOT.65", "EMP.65", "UNEMP.65", "CIVIL.65",
                      "LF.1619.P", "NLF.1619.P", "TOT.1619.P", "EMP.1619.P", "UNEMP.1619.P", "CIVIL.1619.P",
                      "LF.2064.P", "NLF.2064.P", "TOT.2064.P", "EMP.2064.P", "UNEMP.2064.P", "CIVIL.2064.P",
                      "LF.65.P", "NLF.65.P", "TOT.65.P", "EMP.65.P", "UNEMP.65.P", "CIVIL.65.P")]
    
   f.ctyEMP_cty <- bind_rows(f.ctyEMP_agy, f.ctyEMP_cty)
   
}
 f.ctyEMP_cty$geoname <- sub(", Colorado","",f.ctyEMP_cty$geoname)  
 f.ctyEMP_cty[is.na(f.ctyEMP_cty)] <- 0
 ctyList <- as.list(unique(sort(f.ctyEMP_cty$county)))

 # preparing files
     f.ctyEMP_tot <- f.ctyEMP_cty[, c(1:20)]
     f.ctyEMP_pct <- f.ctyEMP_cty[,c(1, 2, 21:38)]
     
     f.ctyEMPL_tot <- f.ctyEMP_tot %>% 
          gather(var, count, LF.1619:CIVIL.65, factor_key=TRUE) %>%
          separate(var,c("type","age_cat"))
     
      f.ctyEMPL_pct <- f.ctyEMP_pct %>% 
          gather(var, pct, LF.1619.P:CIVIL.65.P, factor_key=TRUE) %>%
          separate(var,c("type","age_cat",NA))

# Revising Type 
    f.ctyEMPL_tot$type <- plyr::revalue(f.ctyEMPL_tot$type,  c("LF"="In Labor Force",
                         "NLF" = "Not in Labor Force",
                         "EMP" = "Employed",
                         "UNEMP" = "Unemployed",
                         "CIVIL" = "In Civilian Labor Force",
                         "TOT" = "Total"))
  
    
   f.ctyEMPL_pct$type <- plyr::revalue(f.ctyEMPL_pct$type,  c("LF"="In Labor Force",
                         "NLF" = "Not in Labor Force",
                         "EMP" = "Employed",
                         "UNEMP" = "Unemployed",
                         "CIVIL" = "In Civilian Labor Force",
                         "TOT" = "Total"))
   
 

    
# revising Age Cat
    f.ctyEMPL_tot$age_cat <-plyr::revalue(f.ctyEMPL_tot$age_cat, c("1619" = "16 to 19",
                             "2064" = "20 to 64",
                             "65" = "65+"))
    
  f.ctyEMPL_pct$age_cat <-plyr::revalue(f.ctyEMPL_pct$age_cat, c("1619" = "16 to 19",
                             "2064" = "20 to 64",
                             "65" = "65+"))
    
  

    # Plotly  
    f.ctyEMPL_PLOT <- full_join(f.ctyEMPL_tot,f.ctyEMPL_pct[,2:5],by=c("county","type","age_cat"))
    f.ctyEMPL_LF <- f.ctyEMPL_PLOT[which(f.ctyEMPL_PLOT$type == "In Civilian Labor Force" & f.ctyEMPL_PLOT$age_cat != "Total"),] %>%
         arrange(factor(county, levels = ctyList))
    
    f.ctyEMPL_LF$indText  <- paste0( f.ctyEMPL_LF$geoname," Age Category: ", f.ctyEMPL_LF$age_cat," Percentage: ",percent(f.ctyEMPL_LF$pct * 100)," Count: ",NumFmt(f.ctyEMPL_LF$count))  
    grTitleLF <- paste0("Age Distribution by Percentage in Civilian Labor Force, ",listID$plName1)
 
 f.ctyEMPL_plt <- f.ctyEMPL_PLOT[which(f.ctyEMPL_PLOT$type == "Unemployed"& f.ctyEMPL_PLOT$age_cat != "Total"),] %>%
           arrange(factor(county, levels = ctyList))
    
 f.ctyEMPL_plt$indText  <- paste0( f.ctyEMPL_plt$geoname," Age Category: ", f.ctyEMPL_plt$age_cat," Percentage: ",percent(f.ctyEMPL_plt$pct * 100)," Count: ",NumFmt(f.ctyEMPL_plt$count))  
 grTitle <- paste0("Age Distribution by Percentage Unemployed, ",listID$plName1)
 xAxis <- list(title= "Age Category")
 yAxis <- list(title = 'Percent',tickformat = ".1%")
 
 
# People in Labor Force
if(length(ctyfips) > 1 ){
 LFPlot <- f.ctyEMPL_LF %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~pct,
  #  color=~type,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyEMPL_LF$geoname)[1]
      )
  )) %>% layout( title=grTitleLF, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4) ,
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[1]),
               label = unique(f.ctyEMPL_LF$geoname)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[2]),
               label = unique(f.ctyEMPL_LF$geoname)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[3]),
               label = unique(f.ctyEMPL_LF$geoname)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[4]),
               label = unique(f.ctyEMPL_LF$geoname)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[5]),
               label = unique(f.ctyEMPL_LF$geoname)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[6]),
               label = unique(f.ctyEMPL_LF$geoname)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[7]),
               label = unique(f.ctyEMPL_LF$geoname)[7])
      )
  )))
} else {
   LFPlot <- f.ctyEMPL_LF %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~pct,
 #   color=~type,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyEMPL_LF$geoname)[1]
      )
  ))   %>% layout( title=grTitleLF, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
}    
    
# Unemployed        
if(length(ctyfips) > 1 ){
 UEMPPlot <- f.ctyEMPL_plt %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~pct,
  #  color=~type,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyEMPL_plt$geoname)[1]
      )
  )) %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4),
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_plt$geoname)[1]),
               label = unique(f.ctyEMPL_plt$geoname)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_plt$geoname)[2]),
               label = unique(f.ctyEMPL_plt$geoname)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_plt$geoname)[3]),
               label = unique(f.ctyEMPL_plt$geoname)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_plt$geoname)[4]),
               label = unique(f.ctyEMPL_plt$geoname)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_plt$geoname)[5]),
               label = unique(f.ctyEMPL_plt$geoname)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_plt$geoname)[6]),
               label = unique(f.ctyEMPL_plt$geoname)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_plt$geoname)[7]),
               label = unique(f.ctyEMPL_plt$geoname)[7])
      )
  )))
} else {
   UEMPPlot <- f.ctyEMPL_plt %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~pct,
 #   color=~type,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyEMPL_plt$geoname)[1]
      )
  ))   %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
}    

 
    # flex Table and output data file
 
    f.ctyEMPL_tot$count <- format(round(f.ctyEMPL_tot$count ,digits=0),  big.mark=",")
    f.ctyEMPL_pct$pct <- percent(f.ctyEMPL_pct$pct * 100)
    
     f.ctyEMPL_tot$type2 <- "Count"
     f.ctyEMPL_pct$type2 <- "Percentage"
    
    f.ctyEMP_Count <-  f.ctyEMPL_tot %>% spread(type,count)
    f.ctyEMP_Percent <-  f.ctyEMPL_pct %>% spread(type,pct)
   
    f.ctyEMP_tab <- bind_rows(f.ctyEMP_Count,f.ctyEMP_Percent)  
    # reordering Records for Table
    
    f.ctyEMP_tab  <- f.ctyEMP_tab[,c(1:4,7:9,5,10,6)] %>% arrange(factor(county, levels = ctyList),  
                                              age_cat, desc(type2))
    
    

    
    #Clearing geoname
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
    
    f.ctyEMP_tab <- clrGeoname(f.ctyEMP_tab,"geoname",npanel1,6)
      for(i in 1:nrow(f.ctyEMP_tab)){
      if(i %% 2 == 0){
        f.ctyEMP_tab[i,3] <- ""
      } 
      }
    
    f.ctyEMP_tab<- f.ctyEMP_tab[,c(1,3:10)]
     
    names(f.ctyEMP_tab)[1] <- "Agency/County"
    names(f.ctyEMP_tab)[2] <- "Age Category"
    names(f.ctyEMP_tab)[3] <- "Value"
    
     #Producing Flextable
 
 tab_head <- paste0("Age Distribution by Employment Status, ",listID$plName1)

 

   
   f.flexEMP <- flextable(
       f.ctyEMP_tab,
       col_keys = names(f.ctyEMP_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=9) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=9) %>%
       align(j=1:2, align="left", part="body") %>%
       width(j= 1, width=3) %>%
       width(j=2, width=1) %>%
       width(j=3:6,width=0.7) %>%
       width(j=7:9,width=1) %>%
       height(part="footer", height=0.4) %>%
       height(part="header", i=2,height=1.5)
 

  outList <- list("LFPlot" = LFPlot, "UEMPPlot" =  UEMPPlot,"FlexTable" = f.flexEMP, "data" = f.ctyEMPL_PLOT, "table" = f.ctyEMP_tab,"caption" = outCap)
  return(outList)
}


