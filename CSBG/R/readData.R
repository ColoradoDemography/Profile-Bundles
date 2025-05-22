#' readData Reads data files from SDO and the Censis API
#'
#' @param DBPool the The DOLA database Pool
#' @param curYr the Current data vintage Year
#' @param censKey Census API Key
#' @export


readData <- function(DBPool,curYr,censKey) {

  ACS_Tables <- c('C17002', 'B17003','B17001','C18130', 'B17010', 'B17019', 'B22003', 'C27016')
  outList <- list()
  # Genrate SDO SYA data
  SQLUrl <- paste0("SELECT countyfips, county, year, age, malepopulation, femalepopulation, totalpopulation FROM estimates.county_sya WHERE year = ",curYr,";")
  f.COPop <- dbGetQuery(DBPool, SQLUrl) 
  f.COPop <- f.COPop %>% 
     mutate(GEOID = paste0("08",str_pad(f.COPop$countyfips,3,pad="0")),
            countyname = ifelse(county == "Colorado","Colorado",paste0(county, " County")))

  f.COPop <- f.COPop %>% select(GEOID, countyfips, countyname, year, age, malepopulation, femalepopulation, totalpopulation)
  
  # Reading total US SYA data
  f.USage <- read_csv("data/nc-est2023-agesex-res.csv") 
  agecol <- ncol(f.USage)
  
  f.USPop <- f.USage %>%  
  mutate(countyfips = -100,
         countyname = "United States",
         year = curYr,
         totalpopulation = .[[agecol]],
         GEOID ='01000') %>%
  select(GEOID, countyfips, countyname, year, AGE,  SEX, totalpopulation)

  outList[["US"]] <- f.USPop
  outList[["CO"]] <- f.COPop 

# Generating ACS data
  for(i in 1:length(ACS_Tables)) {
    tabName <- ACS_Tables[i]
  # US Data
    f.US <- get_acs(geography = "us", 
                         table = tabName, 
                         survey = "acs5",
                         year = curYr,
                         key = censKey,
                         output ="wide") %>%
    mutate(GEOID = paste0("0",GEOID,"000"))
  
  # CO data
    f.CO <- get_acs(geography = "state", 
                         table = tabName, 
                         survey = "acs5",
                         state ="CO",
                         year = curYr,
                         key = censKey,
                         output ="wide") %>%
    mutate(GEOID = paste0(GEOID,"000"))
  
  # County Data
    f.CTY <- get_acs(geography = "county", 
                         table = tabName, 
                         survey = "acs5",
                         state ="CO",
                         year = curYr,
                         key = censKey,
                         output ="wide")
   f.ACS_Data <- bind_rows(f.US, f.CO, f.CTY)

   outList[[tabName]] <- f.ACS_Data 
  } #i
  

  return(outList) 
}




