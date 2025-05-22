#pctMOE  Function for Calculating Aggregate Percentage MOE ACS Handbook Chapter 8, PG 6

pctMOE <- function(Y,moeY,moeX,P) {
  fractionY <- 1/Y
  moeXsq <- moeX^2
  Psq <- P^2
  moeYsq <- moeY^2
  moePsq <- moeXsq - (Psq * moeYsq)
  moeP <- sqrt(abs(moePsq))  * fractionY
  return(moeP)
}