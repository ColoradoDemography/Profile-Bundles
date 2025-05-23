#'  CountyName : Produces a vector of County Names from an input list of Census County and Place Name Codes.
#'
#' @param  infips List of county fips codes
#' @return the List of County names for  the infips list
#' @export

CountyName <- function(infips){

  outname <- c()
  for(i in 1:length(infips)){
    tmpname <- switch(infips[i],
                      "08001"	= "Adams County",
                      "08003"	= "Alamosa County",
                      "08005"	= "Arapahoe County",
                      "08007"	= "Archuleta County",
                      "08009"	= "Baca County",
                      "08011"	= "Bent County",
                      "08013"	= "Boulder County",
                      "08014"	= "Broomfield County",
                      "08015"	= "Chaffee County",
                      "08017"	= "Cheyenne County",
                      "08019"	= "Clear Creek County",
                      "08021"	= "Conejos County",
                      "08023"	= "Costilla County",
                      "08025"	= "Crowley County",
                      "08027"	= "Custer County",
                      "08029"	= "Delta County",
                      "08031"	= "Denver County",
                      "08033"	= "Dolores County",
                      "08035"	= "Douglas County",
                      "08037"	= "Eagle County",
                      "08039"	= "Elbert County",
                      "08041"	= "El Paso County",
                      "08043"	= "Fremont County",
                      "08045"	= "Garfield County",
                      "08047"	= "Gilpin County",
                      "08049"	= "Grand County",
                      "08051"	= "Gunnison County",
                      "08053"	= "Hinsdale County",
                      "08055"	= "Huerfano County",
                      "08057"	= "Jackson County",
                      "08059"	= "Jefferson County",
                      "08061"	= "Kiowa County",
                      "08063"	= "Kit Carson County",
                      "08065"	= "Lake County",
                      "08067"	= "La Plata County",
                      "08069"	= "Larimer County",
                      "08071"	= "Las Animas County",
                      "08073"	= "Lincoln County",
                      "08075"	= "Logan County",
                      "08077"	= "Mesa County",
                      "08079"	= "Mineral County",
                      "08081"	= "Moffat County",
                      "08083"	= "Montezuma County",
                      "08085"	= "Montrose County",
                      "08087"	= "Morgan County",
                      "08089"	= "Otero County",
                      "08091"	= "Ouray County",
                      "08093"	= "Park County",
                      "08095"	= "Phillips County",
                      "08097"	= "Pitkin County",
                      "08099"	= "Prowers County",
                      "08101"	= "Pueblo County",
                      "08103"	= "Rio Blanco County",
                      "08105"	= "Rio Grande County",
                      "08107"	= "Routt County",
                      "08109"	= "Saguache County",
                      "08111"	= "San Juan County",
                      "08113"	= "San Miguel County",
                      "08115"	= "Sedgwick County",
                      "08117"	= "Summit County",
                      "08119"	= "Teller County",
                      "08121"	= "Washington County",
                      "08123"	= "Weld County",
                      "08125"	= "Yuma County"
          )
   outname <- c(outname,tmpname)  
  }

  return(outname)
}
  