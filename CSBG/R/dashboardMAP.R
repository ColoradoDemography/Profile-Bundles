#' dashboardMAP Creates a simple map that highlights a Colorado County or place
#'   Modified from cp_countymap  AB 2/2018
#'   Revised 3/2018 to account for standalone JSON dataset
#'
#' This function creates a map to be used in the profile process,
#'    If a planning region is selected, the plannign region is colored in
#'    If a county is selected, the county is colored in and the planning region is outlined
#'    if a place is selected, the county is outlined and a dagger is posted at the center of the place.
#'
#'
#' @param listID the list containing place id and Place names
#' @export

dashboardMAP <- function(listID){
  # Collecting place ids from  idList, setting default values
  entityname <- listID$plName1
  ctyName <- listID$plName2
  ctyList <- listID$list2

  #Accessing JSON file, with Counties 
  data_file <- "www/County_GEN_2014.geojson"
  data_json <- read_sf(data_file)

 selcty <- data_json %>% filter(GEOID %in% ctyList)
 map_title <- paste0("CSBG Entity: ",entityname)
 

 m <- ggplot(data_json) +
    geom_sf(data=data_json, fill = "white", color = "black") +
    geom_sf(data=selcty, fill = "lightblue") +
    geom_sf_text(data=selcty,aes(label = NAME, geometry = geometry), size= 3) +
    labs(title = map_title,
         alt = map_title) +
      theme(plot.title = element_text(hjust = 0.5, size=12),
            panel.background = element_rect(fill = "white", colour = "gray50"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  
  return(m)
}