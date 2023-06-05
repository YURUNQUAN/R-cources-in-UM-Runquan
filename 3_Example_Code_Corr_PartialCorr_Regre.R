#Runquan Michael Yu Create 24/3/2023, add section 0: 25/3/2023, add section 1 and 2: 28/3/2023 
------------------------------------------------------------------------------------------------------------------
#Section 0: 
#Install.packages:if need, delete the "#" and run the code line by line: from Line 4 to Line 6. 
#install.packages("tidyverse")
#install.packages("imager")
#install.packages("bmp")
#install.packages('magrittr')
#install.packages("plotrix")
#install.packages("openxlsx")
#Library: all
library(tidyverse)
library(magrittr)
library(readxl)
library(imager)
require(bmp)
library(plotrix)
library(openxlsx)
-------------------------------------------------------------------------------------------------------------------
#1. luminance (Mean Average Process) by plotrix package
luminance <- function(path) {
  #read the bmp
  img <- read.bmp(path)
  # R G B average
  lum <- apply(img, c(1, 2), function(x) mean(x[1:3]))
  # return mean luminance
  return(mean(lum))
}
#1.1 Paths and pip: 
results <- vector("numeric", 300)
for (i in 0:299) {
  path <- paste0("C:/Users/yurun/Desktop/300 Faces (nine traits) (2)/bmp/f42887_e_", sprintf("%03d", i), ".bmp")
  mean_lum <- luminance(path)
  results[i+1] <- mean_lum  #Note: The vector starts counting from 1
  cat(paste0("Result ", i+1, ": ", mean_lum, "\n"))
}
result_df <- data.frame(Result = 0:299, Mean_Luminance = results)
#Create a new .xlsx document
wb <- createWorkbook()
#Create a new sheet
addWorksheet(wb, sheetName = "Result")
#Write in 
writeData(wb, sheet = "Result", x = result_df)
#1.2 Save the results
saveWorkbook(wb, file.path("C:/Users/yurun/Desktop/NUS/300 Faces (nine traits) (2)", "results.xlsx"), overwrite = TRUE)
-------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(effsize)
library(pwr)
library(tibble) 
library(ggpubr)
library(gridExtra)
options(scipen = 200)
#2.1 Correlation analysis and Regression: 
#2.1.1 Read_excel: 
facetraits <-  as.data.frame(read_excel('C:/Users/yurun/Desktop/300 Faces (nine traits) (2)/300_OriginalFaces_Trait&Gender_Data_Clean.xlsx'))
facetraits <- facetraits[ -nrow(facetraits) ,c( 3 : ( ncol(facetraits)-1 ) )] %>% dplyr::rename('sexprop' = 'Proportion (0=male, 1=female)')

#2.2 Correlations: 
#Correlation 1: pupil & Threatening
PT <- ggscatter(facetraits,x="Mean",y="Threatening",color="#009999",size=1,xlab="Pupil Mean",ylab="Rating of Threating",add="reg.line",add.params = list(color="black",fill="gray"),conf.int=TRUE)
PT
PT+stat_cor()
#Correlation 2: pupil & Luminance
ML <- ggscatter(facetraits,x="Mean",y="Mean_Luminance",color="#009999",size=1,xlab="Pupil Mean",ylab="Luminance",add="reg.line",add.params = list(color="black",fill="gray"),conf.int=TRUE)
ML
ML+stat_cor()
#Correlation 3:Threatening & Luminance
TL <- ggscatter(facetraits,x="Threatening",y="Mean_Luminance",color="#009999",size=1,xlab="Rating of Threating",ylab="Luminance",add="reg.line",add.params = list(color="black",fill="gray"),conf.int=TRUE)
TL
TL+stat_cor()
