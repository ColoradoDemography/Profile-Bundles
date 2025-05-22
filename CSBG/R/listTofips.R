#'  listTofips : Produces a vector of FIPS codes from an input list of Census County and Place Name Codes.
#'
#' @param  lvl the comparison level ("Regional Summary","Region to County","County to County","Municipality to Municipality")
#' @param inlist1 The data frame for the base of comparison
#' @param value1  The selected item for the base of the comparison
#' @param inlist2 The data frame for the comparison
#' @param value2  The selected item(s) for the comparisons
#' @return the fipscode(s) for a selected data level
#' @export

listTofips <- function(value1){
  # Function to produce a vector of FIPS codes from an input list of names and codes
           reglist <- switch(value1,
                             "Adams County" =  c("08001"),
                             "Arapahoe County" =  c("08005"),
                             "Baca County" =  c("08009"),
                             "Balance of State" =  c("08037","08055","08071","08081","08085","08091","08107","08113"),
                             "Boulder County" =  c("08013"),
                             "Broomfield City and County" =  c("08014"),
                             "Colorado East Community Action Agency" =  c("08017","08039","08063","08073"),
                             "Delta County Health Department" =  c("08029"),
                             "Denver City and County" =  c("08031"),
                             "Douglas County" =  c("08035"),
                             "El Paso County" =  c("08041"),
                             "Garfield County Department of Human Services" =  c("08045", "08097"),
                             "Gunnison County CSBG" =  c("08051", "08053"),
                             "Housing Solutions for the Southwest" =  c("08007","08033","08067","08083","08111"),
                             "Jefferson County CSBG" =  c("08059","08093","08119"),
                             "Kiowa County" =  c("08061"),
                             "Larimer County" =  c("08069"),
                             "Mesa County" =  c("08077"),
                             "Mountain Family Center" =  c("08049","08057"),
                             "Northeastern Colorado ALG" =  c("08075","08087","08095","08115","08121","08125"),
                             "Otero County Department of Human Services" =  c("08011","08025","08089"),
                             "Prowers County" =  c("08099"),
                             "Pueblo County" =  c("08101"),
                             "Rio Blanco County" =  c("08103"),
                             "SLV Community Solutions" =  c("08003","08021","08023","08079","08105","08109"),
                             "Summit County CSBG" =  c("08019","08047","08117"),
                             "Upper Arkansas Area Council of Governments" =  c("08015","08027","08043","08065"),
                             "Weld County" =  c("08123")
           )
          fipsl <- list("length1" = length(reglist),"plName1" = value1, "list1"= reglist,"length2" = 0, "plName2" = CountyName(reglist), "list2" = reglist)

  return(fipsl)
} #end listTofips
