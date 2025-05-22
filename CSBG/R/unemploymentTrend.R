#' unemploymentTrend generates tables and ployly charts BLS unemployment data
#'  CSBG Dashboard 11/2019  A. Bickford
#' @param DBPool the DOLA database pool
#' @lvl the selected agenct
#' @param listID is the list of selected county codes
#' @return plotly graphic, data table and data file
#' @export
#' 


unemploymentTrend <- function(listID,curYr,bls_key, barCol, barCol2){
  # Collecting List of Counties

  cty_str <- listID$list2
  cty_name <- CountyName(cty_str)
  cty_num <- as.numeric(str_sub(cty_str,3,5))
  name1 <- listID$plName1
  begYr <- curYr -2
  
# reading data series and selecting counties
  # National and Colorado Unemployment rate
  serieslist <- list("LNU01000000", 	"LNU03000000", "LAUST080000000000004","LAUST080000000000006")
  for(cty in cty_str) {
    unemp <-  paste0("LAUCN", cty, "0000000004")  # Unemployment
    laborforce <- paste0("LAUCN", cty,"0000000006") # Labor force
    serieslist <- c(serieslist,  unemp, laborforce)
  }
  
  
  bls_results <- get_n_series(
    series_ids = serieslist ,
    api_key = bls_key,
    start_year = begYr,
    end_year = curYr,
    year_limit = NULL,
    parse_values = TRUE)
  
  bls_tables <- lapply(bls_results, function(x) data_as_table(x$data))
  f.bls_table <- merge_tables(bls_tables) 
  blscols <- ncol(f.bls_table)
  f.bls_data <- f.bls_table %>%
    gather(series, count, names(f.bls_table)[3]:names(f.bls_table)[blscols], factor_key=TRUE) %>%
    mutate(LOC = str_sub(series,1,5),
           FIPS = ifelse(LOC == "LNU01","-100",
                         ifelse(LOC == "LNU03", "-100",
                                ifelse(LOC == "LAUST","08",str_sub(series,6,10)))))
  
  f.bls_data$NAME <- apply(X = f.bls_data[, 6], MARGIN = 1, FUN = CountyName)
  f.bls_data <- f.bls_data %>% mutate(NAME = ifelse(FIPS == "-100","United States",
                                                    ifelse(FIPS == "08","Colorado",NAME)),
                                      NAME = unlist(NAME),
                                      TYPE = ifelse(series == "LNU03000000","unemp",
                                                    ifelse(series == "LNU01000000","lfp",
                                                           ifelse(grepl("00004", series),"unemp",'lfp')))) 
  
  f.bls_unemp <- f.bls_data %>% filter(TYPE == "unemp") %>%
    mutate(UNEMP = count) %>%
    select(FIPS, NAME, year, period, UNEMP)
  
  f.bls_lfp <- f.bls_data %>% filter(TYPE == "lfp") %>%
    mutate(LFP = count) %>%
    select(FIPS, year, period, LFP)
  
  f.blsdata <- inner_join(f.bls_unemp, f.bls_lfp, by=c("FIPS", "year", "period")) 
  #Calculating the CSBG Entity
  if(length(cty_str) > 1){
    f.blstmp <- f.blsdata %>% filter(!(FIPS %in% c("-100","08")))
    f.blsus <- f.blsdata %>% filter(FIPS == '-100')
    f.blsco <- f.blsdata %>% filter(FIPS == '08')
    
    f.blscsbg <- f.blstmp %>%
      group_by(year, period) %>%
      summarise(UNEMP = sum(UNEMP),
                LFP = sum(LFP)) %>%
      mutate(FIPS = '08.5', 
             NAME = name1) %>%
      select(FIPS, NAME, year, period, UNEMP, LFP)
    f.blsdata <- bind_rows(f.blsus, f.blsco, f.blscsbg, f.blstmp)
  }
  
  f.blsdata <- f.blsdata %>% mutate(UNEMPRATE = (UNEMP/LFP) * 100,
                                    DATE = as.Date(ISOdate(year, as.numeric(str_sub(period,2,3)), 1)))
  
  #Creating Chart
  chart_list <- list()
  unemp_caption <- captionSrc("BLS","","")
  unemp_title <- "Monthly Unemployment Rate"
  unemp_sub <- "CSBG Comparison"
 
  if(length(cty_str) > 1) {
    f.chart_data <- f.blsdata %>% filter(!(FIPS %in% cty_str))
  } else {
    f.chart_data <- f.blsdata
  }
  
  maxlim <- max(f.chart_data$UNEMPRATE)

  
  unempchart <- f.chart_data %>% ggplot(aes(x=DATE, y=UNEMPRATE, group=NAME, color=NAME)) +
    geom_line(linewidth=1.2) +
    scale_x_date(date_labels="%b, %y",date_breaks  ="2 month") +
    scale_y_continuous(limits = c(0, maxlim), label=percent, expand = c(0, 0)) +
    scale_color_manual(values=barCol) +
    labs(title = unemp_title,
         subtitle = unemp_sub,
         caption = unemp_caption,
         x = "Month",
         y= "Unemployment Rate",
         color="CSBG Entity",
         alt = unemp_title) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(plot.title = element_text(hjust = 0.5, size=12),
          plot.caption = element_text(hjust = 0, size=9),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_blank(),
          axis.text.x = element_text(size=10, angle = 45, vjust = 0.9, hjust=1),
          axis.text.y=element_text(size=10),
          legend.position = "bottom", 
          legend.title = element_text(size=8), #change legend title font size
          legend.text = element_text(size=8)) #change legend text font size
  
  chart_list[[1]] <- unempchart
  
  if(length(cty_str) > 1) {
  for(i in 1:length(cty_str)) {
    n <- i + 1
    f.chart_data <- f.blsdata %>% filter(FIPS == "08.5" | FIPS == cty_str[i]) 
    unemp_sub <- "County Comparison"
    maxlim <- 11
    
    unempchart <- f.chart_data %>% ggplot(aes(x=DATE, y=UNEMPRATE, group=NAME, color=NAME)) +
      geom_line(linewidth=1.2) +
      scale_x_date(date_labels="%b, %y",date_breaks  ="2 month") +
      scale_y_continuous(limits = c(0, maxlim), label=percent, expand = c(0, 0)) +
      scale_color_manual(values=barCol2) +
      labs(title = unemp_title,
           subtitle = unemp_sub,
           caption = unemp_caption,
           x = "Month",
           y= "Unemployment Rate",
           color="CSBG Entity and County",
           alt=unemp_title) +
      guides(color=guide_legend(nrow=2, byrow=TRUE))  +
      theme(plot.title = element_text(hjust = 0.5, size=12),
            plot.caption = element_text(hjust = 0, size=9),
            panel.background = element_rect(fill = "white", colour = "gray50"),
            panel.grid.major = element_blank(),
            axis.text.x = element_text(size=10, angle = 45, vjust = 0.9, hjust=1),
            axis.text.y=element_text(size=10),
            legend.position = "bottom", 
            legend.title = element_text(size=8), #change legend title font size
            legend.text = element_text(size=8)) #change legend text font size
    
    chart_list[[n]] <- unempchart
  }
  }
  # Creating dataset for table
  
  f.blsdata_unemp <- f.blsdata %>% 
    mutate(UNEMP = ifelse(FIPS == '-100',UNEMP * 1000,UNEMP),
           NAME = "",
           DATE_STR = format(DATE,"%B, %Y"),
           TYPENO = 2,
           TYPE = "Unemployed",
           UNEMP = comma_conv(UNEMP)) %>% select(FIPS, TYPENO, NAME, TYPE, DATE_STR, UNEMP) %>% 
    pivot_wider(names_from = DATE_STR, values_from = UNEMP) 
  
  
  f.blsdata_lfp <- f.blsdata %>% 
    mutate(LFP = ifelse(FIPS == '-100',LFP * 1000,LFP),
           NAME = "",
           LFP = comma_conv(LFP),
           DATE_STR = format(DATE, "%B, %Y"),
           TYPENO = 3,
           TYPE = "Labor Force Participation") %>% select(FIPS, TYPENO, NAME, TYPE, DATE_STR, LFP) %>% 
    pivot_wider(names_from = DATE_STR, values_from = LFP)  
  
  
  f.blsdata_unemprate <- f.blsdata %>% 
    mutate(TYPE = "Unemployment Rate",
           TYPENO = 1,
           DATE_STR = format(DATE,"%B, %Y"),
           UNEMPRATE = percent(UNEMPRATE)) %>%
    select(FIPS, TYPENO, NAME, TYPE, DATE_STR, UNEMPRATE) %>% 
    pivot_wider(names_from = DATE_STR, values_from = UNEMPRATE) 
  
  f.blsdata_tab <- bind_rows(f.blsdata_unemprate, f.blsdata_unemp, f.blsdata_lfp) %>%
    arrange(FIPS, TYPENO) 

  
  f.blsdata_tab  <- f.blsdata_tab [,3:ncol(f.blsdata_tab)]

  
  names(f.blsdata_tab)[1] <- "CSBG Entity"
  names(f.blsdata_tab)[2] <- "Measure"

  headers <- t(as.data.frame(names(f.blsdata_tab)))
  
  tab_head <- names(f.blsdata_tab)
  nCol <- ncol(f.blsdata_tab)
  f.unempFlex <- flextable(
    f.blsdata_tab,
    col_keys = names(f.blsdata_tab)) %>%
    add_footer_row(values=captionSrc("BLS","",""),top=FALSE,colwidths=nCol) %>%
    align(j=1:nCol, align="center",part="header") %>%
    align(j=1:2, align="left", part="body") %>%
    align(j=3:nCol, align="right", part="body") %>%
    width(j= 1, width=3) %>%
    width(j=2, width=2) %>%
    width(j=3:nCol,width=1.5) %>%
    height(part="footer", height=0.4) %>%
    height(part="body", height=0.5)
    
  #bind list
  outList <- list("plot"= chart_list, "data" = f.blsdata_tab, "header_sh"= headers, "FlexTable" = f.unempFlex)
  
  return(outList)
}