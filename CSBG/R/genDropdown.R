# genDropdown populates the a dropdown list based on the number of 
# elements in the NameList parameter
#Creates a dynamic set of dropdowns

genDropdown <- function(NameList) {
  outlist <- list(list(
    method = "restyle", 
    args=list("transforms[0].value", NameList[1]),
    label = NameList[1]
  ))
  
  for(i in 2:length(NameList)) {
    item <- list(list(
      method = "restyle", 
      args=list("transforms[0].value", NameList[i]),
      label = NameList[i]
    ))
    outlist <- c(outlist,item)
  }
  
  return(outlist)
}