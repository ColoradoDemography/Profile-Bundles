#' agePlotPRO Generates an overall age table using US Population Estimates by single year of Age, Colorado Population estimates
#' CSBG estimates and County Estimates
#'
#' @param DBPool the active database pool
#' @param lvl is the data type 
#' @param listID is the list of selected county codes
#' @param curYr is the single year value to be extracted by county_sya
#' @return plotly graphic, data table and data file
#' @export

agePlotPRO  <- function(inData,listID,curYr,barCol) {

  cty_str <- listID$list2
  cty_name <- CountyName(cty_str)
  cty_num <- c(0,as.numeric(str_sub(cty_str,3,5)))
  name1 <- listID$plName1

  # Collecting US Data 

  f.USage_sex <- inData[["US"]] %>%  filter(AGE != 999) %>%
           mutate(year = curYr,
                  age = AGE,
                  sex = ifelse(SEX == 0, "Total", ifelse(SEX == 1,"Male", "Female")),
                  population = totalpopulation) %>%
          select(GEOID, countyname, year, age, sex, population)
  

  
  ctylist2 <- c("08000", unlist(cty_str))

  f.COAge_long <- inData[["CO"]] %>%  filter(GEOID %in% ctylist2) %>%
    pivot_longer(cols = c('malepopulation', 'femalepopulation', 'totalpopulation'),
                                            names_to='sex',
                                            values_to='population') %>%
    mutate(sex = ifelse(sex == 'totalpopulation','Total',
                 ifelse(sex == 'malepopulation','Male','Female'))
           ) %>%
    arrange(GEOID, sex, age) %>%
    select(GEOID, countyname, year, age, sex, population)

  if(length(cty_str) > 1){  #Summing up CSBG Entity
    f.csbg_age <- f.COAge_long %>% filter(GEOID %in% cty_str) %>%
          group_by(age, sex) %>%
          summarize(population = sum(population)) %>%
          mutate(GEOID = "080.5",
                 year = curYr,
                 countyname = name1) %>%
         ungroup() %>%
      select(GEOID, countyname, year, age, sex, population)
    f.state_age <- f.COAge_long %>% filter(GEOID == "08000")
    f.cty_age  <- f.COAge_long %>% filter(GEOID %in% cty_str) 
    f.age_sya <- bind_rows(f.USage_sex, f.state_age, f.csbg_age, f.cty_age)
  } else {
    f.age_sya <- bind_rows(f.USage_sex, f.COAge_long)
  }


  # Calculating decades for age pyramids
  f.age_10 <- f.age_sya %>% filter(sex != 'Total')   %>%
          mutate(age10 = cut(age,seq(0,100, by=10),
                         labels=seq(0,9,by=1)),
                 age10 = ifelse(is.na(age10),1,age10)) %>%
         group_by(GEOID, countyname, sex, age10) %>%
         summarize(population = sum(population)) %>%
         ungroup() %>%
         mutate(year = curYr)  %>% arrange(GEOID, age10) %>%
         select(GEOID, countyname, year, sex, age10, population) 
  
  f.age_10_total <- f.age_10 %>%
             group_by(GEOID, countyname, sex) %>%
             summarize(poptotal = sum(population)) %>%
            ungroup() %>%
            select(GEOID, sex, poptotal)
  

  f.age_10_fin <- inner_join(f.age_10, f.age_10_total,by=c('GEOID', 'sex')) %>%
               mutate(pct = (population/poptotal)*100) 
          
  
  f.age_10_fin$age10 <- factor(f.age_10_fin$age10, levels=c(1,2,3,4,5,6,7,8,9,10),
                           labels=c("0 to 10", "11 to 20", "21 to 30", "31 to 40", "41 to 50",
                                    "51 to 60", "61 to 70", "71 to 80", "81 to 90", "91 to 100"))  

  locs <- unique(f.age_10_fin$GEOID)
  ctynames <- unique(f.age_10_fin$countyname)
  charts_list <- list()
  pltTitle <- paste0("CSBG Entity Age Pyramid: ", name1)


  for(i in 1:length(locs)) {

    sexbyagePL <- f.age_10_fin %>% filter(GEOID == locs[[i]]) 
    maxm <- sexbyagePL %>%  filter(sex == "Male") %>% select(pct)
    maxf <- sexbyagePL %>%  filter(sex == "Female") %>% select(pct)
    maxpct <- max(maxm,maxf)
    maxval <- (10*ceiling(maxpct/10)) + 10
    
    sexbyagePL <- sexbyagePL %>% mutate(pct = ifelse(sex == 'Female',pct * -1,pct))
    sexbyagePL$sex <- factor(sexbyagePL$sex,labels=c("Female","Male"))
    
  pyramid <- ggplot(sexbyagePL, aes(x = age10, y = pct, fill = sex)) + 
    geom_bar(data = subset(sexbyagePL, sex == "Female"), color="black", stat = "identity") + 
    geom_bar(data = subset(sexbyagePL, sex == "Male"), color="black", stat = "identity") + 
    scale_fill_manual(values=barCol, name="sex") +
    scale_y_continuous(breaks=seq(-maxval,maxval,10),labels = paste0(as.character(c(seq(maxval, 10, -10), seq(0,maxval,10))), "%")) + 
    coord_flip() +
    labs(title = pltTitle,
         subtitle = ctynames[i],
         caption = captionSrc("SDO","",curYr),
         x = "Age Group",
         y= "Percentage of Population",
         alt=pltTitle) +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text=element_text(size=12),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_blank(),
          legend.position= "bottom")
  charts_list[[i]] <- pyramid
  }      
  
# Create data file in table age groups   
  f.age_tab1 <- f.age_sya %>% filter(sex == 'Total') %>%
            mutate(agecat = ifelse(age < 5,"0004",
                             ifelse(age < 18, "0517",
                             ifelse(age < 25, "1824",
                             ifelse(age < 45, "2544",
                             ifelse(age < 65,"4564",
                             ifelse(age < 75, "6574","75100"))))))) %>%
            group_by(GEOID, countyname,  agecat) %>%
            summarize(population = sum(population)) %>%
            ungroup() %>%
            mutate(year = curYr) %>% arrange(GEOID, agecat) %>%
           select(GEOID, countyname, year, agecat, population) 
          
  
  f.age_total <- f.age_sya %>% filter(sex == 'Total') %>%
    group_by(GEOID) %>%
    summarize(poptotal = sum(population)) %>%
    ungroup() %>%
    select(GEOID, poptotal)
  
  f.age_tab <- inner_join( f.age_tab1,  f.age_total, by='GEOID') %>%
    mutate(percent = population/poptotal) %>%
    select(GEOID, countyname, agecat, population, percent) %>%
    pivot_wider(names_from='agecat', values_from=c("population","percent")) %>%
    group_by(GEOID, countyname) %>%
    mutate(poptotal = sum(population_0004, population_0517, population_1824,	population_2544,	
                          population_4564,	population_6574,	population_75100),
          percenttotal = sum(percent_0004, percent_0517,	percent_1824,	percent_2544,	
                             percent_4564, percent_6574,	percent_75100),
          GEOID <- comma_conv(GEOID),
          poptotal = comma_conv(poptotal),
          population_0004 = comma_conv(population_0004),
          population_0517 = comma_conv(population_0517),
          population_1824 = comma_conv(population_1824),
          population_2544 = comma_conv(population_2544),
          population_4564 = comma_conv(population_4564),
          population_6574 = comma_conv(population_6574),
          population_75100 = comma_conv(population_75100),
          percenttotal= percent(percenttotal * 100),
          percent_0004 = percent(percent_0004 * 100),
          percent_0517 = percent(percent_0517 * 100),
          percent_1824 = percent(percent_1824 * 100),
          percent_2544 = percent(percent_2544 * 100),
          percent_4564 = percent(percent_4564 * 100),
          percent_6574 = percent(percent_6574 * 100),
          percent_75100 = percent(percent_75100 * 100)) %>%
    ungroup() %>%
    select(countyname, 
           population_0004,	percent_0004,	
           population_0517,	percent_0517,	
           population_1824,	percent_1824,	
           population_2544,	percent_2544,	
           population_4564,	percent_4564,	
           population_6574,	percent_6574,	
           population_75100,	percent_75100, 
           poptotal, percenttotal)

  tab_head <- paste0("Population by Age Group: ", name1)

  head_level1 <- c("",	"Age 0 to 4",	"",	"Age 5 to 17",	"",	"Age 18 to 24",	"",	"Age 25 to 44",	"",	
                   "Age 45 to 64",	"",	"Age 65 to 74",	"",	
                   "Age 75 to 100",	"",	"Total",	"")
 
  head_measure <- c("CSBG Entity",rep(c("Population","Percentage"),8))
  headers <-t(bind_cols(as.data.frame(head_level1), as.data.frame(head_measure)))


  caption = captionSrc("SDO","",curYr)
  

 f.ageTable <- flextable(
       f.age_tab,
       col_keys = names(f.age_tab))  %>%
      add_header_row(values =head_measure,top =TRUE) %>%
      add_header_row(values =head_level1,top =TRUE) %>%
      align(j =1:17,align ="center",part ="header") %>%
      delete_rows(i = 3, part = "header") %>%
      merge_at(i =1,j =2:3, part ="header") %>%
      merge_at(i =1,j =4:5, part ="header") %>%
      merge_at(i =1,j =6:7, part ="header") %>%
      merge_at(i =1,j =8:9, part ="header") %>%
      merge_at(i =1,j =10:11, part ="header") %>%
      merge_at(i =1,j =12:13, part ="header") %>%
      merge_at(i =1,j =14:15, part ="header") %>%
      merge_at(i =1,j =16:17, part ="header") %>%
      add_footer_row(values=caption, colwidths= 17, top=FALSE) %>%
      align(j=1, align="left", part="body") %>%
      align(j=2:17, align="right", part="body") %>%
      width(j= 1, width=3) %>%
      width(j=2:17, width=0.75) %>%
      height(part="footer", height=0.4)
 
 f.ageTable <- add_header_lines(f.ageTable,values =tab_head, top =TRUE)

  
  outList <- list("plot" = charts_list, "data" = f.age_tab, 'header_sh' = headers, "FlexTable" = f.ageTable)
  return(outList)
}