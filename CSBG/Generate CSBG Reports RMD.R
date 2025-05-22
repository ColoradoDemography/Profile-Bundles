#' Community Services Block Grant Dashboard
#' @author  Adam Bickford, Colorado State Demography Office, July 2024
#' Adds National and State data where available
#' Changes WIC data source from Kids Counts to CDPHE WIC
#' Release Version 3.0
#' 
# https://www.dataquest.io/blog/r-api-tutorial/
# https://pdf-services-ue1.adobe.io/operation/accessibilitychecker

rm(list = ls())
setwd("C:/Users/abickford/Documents/Profile Reports/CSBG")

library(tidyverse, quietly=TRUE)
library(readr)
library(readxl, quietly=TRUE)
library(scales, quietly=TRUE)
library(openxlsx)

library(knitr, quietly=TRUE)
library(kableExtra, quietly=TRUE)
library(RPostgreSQL)
library(rmarkdown)

library(jsonlite)
library(geojsonR)
library(geojsonio)
library(units)
library(grid)
library(gridExtra)
library(ggthemes)
library(officer)
library(flextable)
library(tidycensus)
library(censusapi)  # Installed 2019)
library(sf)
library(accessr)
library(officedown)
library(blsR)


# Additions for Database pool
library('pool') 
library('DBI')
library('stringr')
library('config')

source("R/API_setup.R")  # Codes setting up ACS version and Census API Key


source("R/ageEmployment.R")
source("R/agePlotPRO.R")
source("R/agePlotPRO2_tab.R")
source("R/boxContent.R")
source("R/captionSrc.R")
source("R/chkID.R")
source("R/clrGeoname.R")
source("R/dashboardMAP.R")
source("R/CountyName.R")
source("R/disabilityPRO.R")
source("R/downloadObj.R")
source("R/downloadObjUI.R")
source("R/educPRO.R")
source("R/familiesPRO.R")
source("R/graph_objects.R")
source("R/insurance.R")
source("R/housingPRO.R")
source("R/listTofips.R")
source("R/NumFmt.R")
source("R/outputWord.R")
source("R/percent.R")
source("R/popPlace.R")
source("R/popTable.R")
source("R/povertyPRO.R")
source("R/povertyPRO2.R")
source("R/povertyTrend.R")
source("R/roundUpNice.R")
source("R/readData.R")
source("R/submitPush.R")
source("R/submitReport.R")
source("R/tabList.R")
source("R/tabTitle.R")
source("R/TempFil.R")
source("R/simpleCap.R")
source("R/snap.R")
source("R/unemploymentTrend.R")
source("R/pctMOE.R")


# Set up database pool 1/23/19

config <- get("database")
DOLAPool <-  dbPool(
  drv <- dbDriver(config$Driver),
  dbname = config$Database,
  host = config$Server,
  port = config$Port,
  user = config$UID,
  password = config$PWD
)


# Install Office2PDF
#install_otp(dir = "accessr", quiet = FALSE)   

#Conversion functions
comma_conv <- function(inval) {
  if(is.numeric(inval)) {
    y <- formatC(inval,format="f", digits=0, big.mark=",")  #Apply Comma Format
  } else {
    y <- inval
  }
  return(y)
}


# Generating list of entities
f.CSBG <- read_excel("data/CSBG Entities 2021 2024.xlsx")
names(f.CSBG) <-c("GEOID","COUNTY","ENTITY2021","ENTITY", "CHANGE","RES")
f.CSBG_1 <- f.CSBG %>% select(ENTITY) %>% distinct(ENTITY) %>% arrange(ENTITY)

barCol <- c("#359A7E", "#6D3A5D", "#007ADE" )
barCol2 <- c("#007ADE", "#C0504D")

# Reading data files
f.datafiles <- readData(DBPool=DOLAPool,curYr=curYr, censKey=censAPI)
poolClose(DOLAPool)
i <- 4

for(i in 1: nrow(f.CSBG_1)) {
     source("R/dashboardMAP.R")
     entity <-f.CSBG_1$ENTITY[i]
     fipslist <- listTofips(value1=entity)
    

     
 # Creating entity objects
     dashboardMAP <- dashboardMAP(listID=fipslist)
     age_tab <- agePlotPRO(inData = f.datafiles,listID=fipslist,curYr=curYr, barCol=barCol2)
     emp_list <- unemploymentTrend(listID=fipslist,curYr=curYr,bls_key=bls_key, barCol=barCol, barCol2=barCol2)
     pov_list <- povertyPRO(listID=fipslist,curYr=curYr, inData=f.datafiles, barCol=barCol, barCol2=barCol2)
     educatt_list <- educPRO(listID=fipslist,curYr=curYr, inData=f.datafiles, barCol=barCol, barCol2=barCol2)
     povage_list <- povertyPRO2(listID=fipslist,curYr=curYr, inData=f.datafiles, barCol=barCol, barCol2=barCol2)
     dis_list <- disabilityPRO(listID=fipslist,curYr=curYr, inData=f.datafiles, barCol=barCol, barCol2=barCol2)
     fam_list <- familiesPRO(listID=fipslist,curYr=curYr, inData=f.datafiles, barCol=barCol, barCol2=barCol2)
     hh_list <- housingPRO(listID=fipslist,curYr=curYr, inData=f.datafiles, barCol=barCol, barCol2=barCol2)
     snap_list <- snap(listID=fipslist,curYr=curYr, inData=f.datafiles, barCol=barCol, barCol2=barCol2)
     insurance_list <- insurance(listID=fipslist,curYr=curYr, inData=f.datafiles, barCol=barCol, barCol2=barCol2)
 
     
# Creating output directories
     outputDir <- paste0(entity)
     dir_names <- c("Age Distribution","Unemployment Trend","Poverty Levels", "Education By Poverty", "Age by Poverty",
                          "Disability by Poverty", "Families by Poverty", "Housing Tenure by Poverty",
                          "SNAP Program by Poverty", "Health Insurance by Poverty")
     
     dir.create(outputDir,recursive = TRUE)
     for(j in 1:length(dir_names)) {
       dir.create(file.path(outputDir, dir_names[j]), recursive = TRUE)
      }
     
# Generating Excel data
     
     # Styles for header, footer and columns
     # Create Style  
     head_S1 <- createStyle(fontSize=12,fontName="Calibri", textDecoration = "bold", halign = "left") #  First Line
     head_S2 <- createStyle(fontSize=12,fontName="Calibri", halign = "left") # 2nd to 4th line
     columnhead_S1 <- createStyle(fontSize=12,fontName="Calibri", halign = "center", textDecoration = "bold",wrapText=TRUE,
                                  border = "TopBottomLeftRight") # Column headers for table  
     
     cells_S1 <- createStyle(fontSize=12,fontName="Calibri", halign = "left")  # Conditional Formatting for Columns 1, 2
     cells_S2 <- createStyle(fontSize=12,fontName="Calibri", halign = "right")  # Conditional Formatting data columns
   
     wb_out <- createWorkbook()
     for(j in 1:length(dir_names)) {
       addWorksheet(wb_out, dir_names[j])
       if(j == 1){
         ncols <- ncol(age_tab[["data"]])
         nrows <- nrow(age_tab[["data"]]) + 7
         writeData(wb_out,j,paste0("Age Distribution, Counties, ",entity), startRow=1,startCol = 1)
         writeData(wb_out,j,captionSrc("SDO",curYr,""), startRow=2,startCol = 1)
         writeData(wb_out,j,age_tab[["header_sh"]], startRow=4,startCol = 1, colNames=FALSE)
         writeData(wb_out,j, age_tab[["data"]], startRow=7,  startCol=1, colNames=FALSE)
         pos1 <- 2
         for(a in 1:8){
           pos2 <- pos1 + 1
           mergeCells(wb_out,j,cols=pos1:pos2,rows=4)
           pos1 <- pos1 + 2
         } 
         addStyle(wb_out, j, style = head_S1, rows = 1:2, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = columnhead_S1, rows = 4:5, cols = 1:ncols, gridExpand = TRUE) # Column headings
         
         addStyle(wb_out, j, style = cells_S1, rows = 7:nrows, cols = 1, gridExpand = TRUE)
         addStyle(wb_out, j, style = cells_S2, rows = 7:nrows, cols = 2:ncols, gridExpand = TRUE)
         # Setting Row heights and Column Width
         setColWidths(wb_out,j,cols=1, widths=22)
       }
       if(j == 2){
         ncols <- ncol(emp_list[["data"]])
         nrows <- nrow(emp_list[["data"]]) + 7
         writeData(wb_out,j,paste0("Unemployment Rate, Counties, ",entity), startRow=1,startCol = 1)
         writeData(wb_out,j,captionSrc("BLS2",curYr,""), startRow=2,startCol = 1)
         writeData(wb_out,j,emp_list[["header_sh"]], startRow=4,startCol = 1, colNames=FALSE)
         writeData(wb_out,j, emp_list[["data"]], startRow=7,  startCol=1, colNames=FALSE)
         
         addStyle(wb_out, j, style = head_S1, rows = 1:2, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = columnhead_S1, rows = 4, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = cells_S1, rows = 7:nrows, cols = 1:2, gridExpand = TRUE)
         addStyle(wb_out, j, style = cells_S2, rows = 7:nrows, cols = 3:ncols, gridExpand = TRUE)
         # Setting Row heights and Column Width
         setColWidths(wb_out,j,cols=1, widths=22)
         setColWidths(wb_out,j,cols=2, widths=25)
       }   
       if(j == 3){
         ncols <- ncol(pov_list[["data"]])
         nrows <- nrow(pov_list[["data"]]) + 7
         writeData(wb_out,j,paste0("Income by Poverty Level, Counties, ",entity), startRow=1,startCol = 1)
         writeData(wb_out,j,captionSrc("ACS",curYr,"C17002"), startRow=2,startCol = 1)
         writeData(wb_out,j,pov_list[["header_sh"]], startRow=4, startCol = 1, colNames=FALSE)
         writeData(wb_out,j, pov_list[["data"]], startRow=7,  startCol=1, colNames=FALSE)
         pos1 <- 2
         for(a in 1:5){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=4)
           pos1 <- pos1 + 4
         } 
         
         addStyle(wb_out, j, style = head_S1, rows = 1:2, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = columnhead_S1, rows = 4:5, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = cells_S1, rows = 7:nrows, cols = 1, gridExpand = TRUE)
         addStyle(wb_out, j, style = cells_S2, rows = 7:nrows, cols = 2:ncols, gridExpand = TRUE)
         # Setting Row heights and Column Width
         setColWidths(wb_out,j,cols=1, widths=22)
       }   
       if(j == 4){
         ncols <- ncol(educatt_list[["data"]])
         nrows <- nrow(educatt_list[["data"]]) + 7
         writeData(wb_out,j,paste0("Educational Attainment by Poverty Status, Counties, ",entity), startRow=1,startCol = 1)
         writeData(wb_out,j,captionSrc("ACS",curYr,"B17003"), startRow=2,startCol = 1)
         writeData(wb_out,j,educatt_list[["header_sh"]], startRow=4,startCol = 1, colNames=FALSE)
         writeData(wb_out,j,educatt_list[["data"]], startRow=7,  startCol=1, colNames=FALSE)
         pos1 <- 3
         for(a in 1:5){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=4)
           pos1 <- pos1 + 4
         } 
         addStyle(wb_out, j, style = head_S1, rows = 1:2, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = columnhead_S1, rows = 4:5, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = cells_S1, rows = 7:nrows, cols = 1:2, gridExpand = TRUE)
         addStyle(wb_out, j, style = cells_S2, rows = 7:nrows, cols = 3:ncols, gridExpand = TRUE)
         # Setting Row heights and Column Width
         setColWidths(wb_out,j,cols=1, widths=22)
         setColWidths(wb_out,j,cols=2, widths=30)
       }    
       if(j == 5){
         ncols <- ncol(povage_list[["data"]])
         nrows <- nrow(povage_list[["data"]]) + 7  
         writeData(wb_out,j,paste0("Age by Poverty Status,  Counties, ",entity), startRow=1,startCol = 1)
         writeData(wb_out,j,captionSrc("ACS",curYr,"B17001"), startRow=2,startCol = 1)
         writeData(wb_out,j,povage_list[["header_sh"]], startRow=4,startCol = 1, colNames=FALSE)
         writeData(wb_out,j,povage_list[["data"]], startRow=7,  startCol=1, colNames=FALSE)
         pos1 <- 3
         for(a in 1:7){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=4)
           pos1 <- pos1 + 4
         } 
         
         addStyle(wb_out, j, style = head_S1, rows = 1:2, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = columnhead_S1, rows = 4:5, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = cells_S1, rows = 7:nrows, cols = 1:2, gridExpand = TRUE)
         addStyle(wb_out, j, style = cells_S2, rows = 7:nrows, cols = 3:ncols, gridExpand = TRUE)
         # Setting Row heights and Column Width
         setColWidths(wb_out,j,cols=1, widths=22)
         setColWidths(wb_out,j,cols=2,width=30)
       }    
       if(j == 6){
         ncols <- ncol(dis_list[["data"]])
         nrows <- nrow(dis_list[["data"]]) + 7  
         writeData(wb_out,j,paste0("Persons with Disabilities by Age and Poverty Status, Counties, ",entity), startRow=1,startCol = 1)
         writeData(wb_out,j,captionSrc("ACS",curYr,"C18130"), startRow=2,startCol = 1)
         writeData(wb_out,j,dis_list[["header_sh"]], startRow=4,startCol = 1, colNames=FALSE)
         writeData(wb_out,j,dis_list[["data"]], startRow=7,  startCol=1, colNames=FALSE)
         pos1 <- 3
         for(a in 1:4){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=4)
           pos1 <- pos1 + 4
         } 
         
         addStyle(wb_out, j, style = head_S1, rows = 1:2, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = columnhead_S1, rows = 4:5, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = cells_S1, rows = 7:nrows, cols = 1:2, gridExpand = TRUE)
         addStyle(wb_out, j, style = cells_S2, rows = 7:nrows, cols = 3:ncols, gridExpand = TRUE)
         # Setting Row heights and Column Width
         setColWidths(wb_out,j,cols=1, widths=22)
         setColWidths(wb_out,j,cols=2, widths=30)         
       }
       if(j == 7){
         ncols <- ncol(fam_list[["data"]])
         nrows <- nrow(fam_list[["data"]]) + 7  
         
         writeData(wb_out,j,paste0("Family Composition by Poverty, Counties, ",entity), startRow=1,startCol = 1)
         writeData(wb_out,j,captionSrc("ACS",curYr,"B17010"), startRow=2,startCol = 1)
         writeData(wb_out,j,fam_list[["header_sh"]], startRow=4,startCol = 1, colNames=FALSE)
         writeData(wb_out,j,fam_list[["data"]], startRow=7,  startCol=1, colNames=FALSE)
         pos1 <- 3
         for(a in 1:4){
           pos2 <- pos1 + 9
           mergeCells(wb_out,j,cols=pos1:pos2,rows=4)
           pos1 <- pos1 + 10
         }
         pos1 <- 3
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=11:12,rows=5)
         pos1 <- 13
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=21:22,rows=5)
         pos1 <- 23
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=31:32,rows=5)
         pos1 <- 33
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=41:42,rows=5)
         
         addStyle(wb_out, j, style = head_S1, rows = 1:2, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = columnhead_S1, rows = 4:6, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = cells_S1, rows = 7:nrows, cols = 1:2, gridExpand = TRUE)
         addStyle(wb_out, j, style = cells_S2, rows = 7:nrows, cols = 3:ncols, gridExpand = TRUE)
         # Setting Row heights and Column Width
         setColWidths(wb_out,j,cols=1, widths=22)
         setColWidths(wb_out,j,cols=2, widths=45)
       }
       if(j == 8){
         ncols <- ncol(hh_list[["data"]])
         nrows <- nrow(hh_list[["data"]]) + 7
         
         writeData(wb_out,j,paste0("Housing Tenure by Poverty Status, Counties, ",entity), startRow=1,startCol = 1)
         writeData(wb_out,j,captionSrc("ACS",curYr,"B17019"), startRow=2,startCol = 1)
         writeData(wb_out,j,hh_list[["header_sh"]], startRow=4,startCol = 1, colNames=FALSE)
         writeData(wb_out,j,hh_list[["data"]], startRow=7,  startCol=1, colNames=FALSE)
         pos1 <- 3
         for(a in 1:4){
           pos2 <- pos1 + 9
           mergeCells(wb_out,j,cols=pos1:pos2,rows=4)
           pos1 <- pos1 + 10
         }
         pos1 <- 3
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=11:12,rows=5)
         pos1 <- 13
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=21:22,rows=5)
         pos1 <- 23
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=31:32,rows=5)
         pos1 <- 33
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=41:42,rows=5)
         
         addStyle(wb_out, j, style = head_S1, rows = 1:2, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = columnhead_S1, rows = 4:6, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = cells_S1, rows = 7:nrows, cols = 1:2, gridExpand = TRUE)
         addStyle(wb_out, j, style = cells_S2, rows = 7:nrows, cols = 3:ncols, gridExpand = TRUE)
         # Setting Row heights and Column Width
         setColWidths(wb_out,j,cols=1, widths=22) 
         setColWidths(wb_out,j,cols=2, widths=35) 
       }
       if(j == 9){
         ncols <- ncol(snap_list[["data"]])
         nrows <- nrow(snap_list[["data"]]) + 7
         
         writeData(wb_out,j,paste0("SNAP Program Participation by Age and Poverty Status, Counties, ",entity), startRow=1,startCol = 1)
         writeData(wb_out,j,captionSrc("ACS",curYr,"B22003"), startRow=2,startCol = 1)
         writeData(wb_out,j,snap_list[["header_sh"]], startRow=4,startCol = 1, colNames=FALSE)
         writeData(wb_out,j,snap_list[["data"]], startRow=7,  startCol=1, colNames=FALSE)
  
         mergeCells(wb_out,j,cols=3:6,rows=4)
         mergeCells(wb_out,j,cols=7:10,rows=4)
         mergeCells(wb_out,j,cols=11:12,rows=4)
 
         addStyle(wb_out, j, style = head_S1, rows = 1:2, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = columnhead_S1, rows = 4:5, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = cells_S1, rows = 7:nrows, cols = 1:2, gridExpand = TRUE)
         addStyle(wb_out, j, style = cells_S2, rows = 7:nrows, cols = 3:ncols, gridExpand = TRUE)
         # Setting Row heights and Column Width
         setColWidths(wb_out,j,cols=1, widths=22)  
         setColWidths(wb_out,j,cols=2, widths=30)  
       }
       if(j == 10){
         ncols <- ncol(insurance_list[["data"]])
         nrows <- nrow(insurance_list[["data"]]) + 7
         
         
         writeData(wb_out,j,paste0("Health Insurance by Age and Poverty by Poverty Status, Counties, ",entity), startRow=1,startCol = 1)
         writeData(wb_out,j,captionSrc("ACS",curYr,"C27016"), startRow=2,startCol = 1)
         writeData(wb_out,j,insurance_list[["header_sh"]], startRow=4,startCol = 1, colNames=FALSE)
         writeData(wb_out,j,insurance_list[["data"]], startRow=7,  startCol=1, colNames=FALSE)
         mergeCells(wb_out,j,cols=3:12,rows=4)
         mergeCells(wb_out,j,cols=13:22,rows=4)
         mergeCells(wb_out,j,cols=23:32,rows=4)
         mergeCells(wb_out,j,cols=33:42,rows=4)
         pos1 <- 3
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=11:12,rows=5)
         pos1 <- 13
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=21:22,rows=5)
         pos1 <- 23
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=31:32,rows=5)
         pos1 <- 33
         for(a in 1:2){
           pos2 <- pos1 + 3
           mergeCells(wb_out,j,cols=pos1:pos2,rows=5)
           pos1 <- pos1 + 4
         }
         mergeCells(wb_out,j,cols=41:42,rows=5)
         
         addStyle(wb_out, j, style = head_S1, rows = 1:2, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = columnhead_S1, rows = 4:6, cols = 1:ncols, gridExpand = TRUE) # Column headings
         addStyle(wb_out, j, style = cells_S1, rows = 7:nrows, cols = 1:2, gridExpand = TRUE)
         addStyle(wb_out, j, style = cells_S2, rows = 7:nrows, cols = 3:ncols, gridExpand = TRUE)
         # Setting Row heights and Column Width
         setColWidths(wb_out,j,cols=1, widths=22)  
         setColWidths(wb_out,j,cols=2, widths=30) 
       }
     }
     
     
     FN <- paste0(outputDir,"/",entity," Data Tables.xlsx")
     saveWorkbook(wb_out,FN, overwrite = TRUE)
     
# Outputting Charts
     fileLoc <- matrix("",nrow=13,ncol=2)
     
     # Outputting Map
     fileName <- paste0(entity, " County Map.png")
     fileLoc[1,1] <- paste0(outputDir,"/",fileName)
     fileLoc[1,2] <- paste0("CSBG County Map: ",entity)
     ggsave(path= outputDir,filename=fileName,dashboardMAP, device="png", bg="white", height = 6 , width = 7, dpi=300)

     for(j in 1:length(dir_names)) {
       if(j == 1){
         chart_desc <- ' Age Distribution'
         out_chart <- age_tab[["plot"]]
       } 
       if(j == 2){
         chart_desc <- ' Unemployment Trend'
         out_chart <- emp_list[["plot"]]
       }   
       if(j == 3){
         chart_desc <- ' Poverty Levels'
         out_chart <- pov_list[["plot"]]
       }   
       if(j == 4){
         chart_desc <- ' Educational Attainment By Poverty Status'
         out_chart <- educatt_list[["plot"]]
       }    
       if(j == 5){
         chart_desc <- ' Age by Poverty Status'
         out_chart <- povage_list[["plot"]]
       }    
       if(j == 6){
         chart_desc <- ' Disability by Poverty Status'
         out_chart <- dis_list[["plot"]]
       }
       if(j == 7){
         chart_desc <- ' Families by Poverty Status'
         out_chart <- fam_list[["plot"]]
       }
       if(j == 8){
         chart_desc <- ' Housing Tenure by Poverty Status'
         out_chart <- hh_list[["plot"]]
       }
       if(j == 9){
         chart_desc <- ' SNAP Program by Poverty Status'
         out_chart <- snap_list[["plot"]]
       }
       if(j == 10){
         chart_desc <- ' Health Insurance by Poverty Status'
         out_chart <- insurance_list[["plot"]]
       }
       filepath <- paste0(outputDir,"/",dir_names[j],"/")
       
    if(j == 1) {
      for(x in 1:length(out_chart)) {
        if(x == 1){
          fileName <- paste0("United States", chart_desc,".png")
          chart_desc <- "Age Distribution, United States"
        }
        if(x == 2){
          fileName <- paste0("Colorado", chart_desc,".png")
          chart_desc <- "Age Distribution, Colorado"
        }
        if(x == 3){
          fileName <- paste0(entity,chart_desc,".png")
          chart_desc <- paste0("Age Distribution, ",entity)
        }
        if(x > 3) {
          fileName <- paste0(fipslist$plName2[x-3], chart_desc,".png")
        }
      fileLoc[x+1,1] <- paste0(filepath,"/",fileName)
      fileLoc[x+1,2] <- paste0(chart_desc)
      
      ggsave(path= filepath,filename=fileName,out_chart[[x]], device="png", bg="white", height = 5 , width = 8, dpi=300)
      }
    } else {
      for(x in 1:length(out_chart)) {
        if(x == 1){
          fileName <- paste0("United States Colorado ",entity, chart_desc,".png")
          fileLoc[j+3,1] <- paste0(filepath,fileName)
          fileLoc[j+3,2] <- paste0("United States Colorado ",entity, chart_desc)
          ggsave(path= filepath,filename=fileName,out_chart[[x]], device="png", bg="white", height = 5 , width = 8, dpi=300)
        } else {
         fileName <- paste0(entity, " and ", fipslist$plName2[x-1],chart_desc,".png")
         ggsave(path= filepath,filename=fileName,out_chart[[x]], device="png", bg="white", height = 5 , width = 8, dpi=300)
        }
        } # x
    }
       

    } # j loop
     
# Generating Report
  
     
     file_pdf <- paste0("CSBG Agency Report ",entity,".pdf")
     reportName <- "CSBG_Report.pdf"
#     rmarkdown::render(input = "CSBG_Report.Rmd",
#                          output_file = file_docx,
#                          params = list("entity" = entity,
#                                         "rpt_title" = paste0("CSBG Agency Report: ",entity," ",curYr),
#                                         "curYr" = curYr,
#                                        "outputDir" =  outputDir,
#                                        "fileLoc" =  fiileLoc),
#                          run_pandoc = TRUE)
     
     params_list <- list("entity" = entity, "rpt_title" = paste0("CSBG Agency Report: ",entity," ",curYr), "curYr" = curYr, "fileLoc" = fileLoc)
     rmd2word("CSBG_Report", pdf = TRUE, params = params_list)
     
# Move report file  
     file.rename(from=paste0("./",reportName),
                 to=paste0("./",outputDir,"/",file_pdf))
     
# Creating ZIP file
     files2zip <- dir(outputDir, full.names = TRUE)
     zip(zipfile = paste0("Zipped Files/",entity), files = files2zip)
     unlink(outputDir, recursive=TRUE)
 }  # i loop     
        

