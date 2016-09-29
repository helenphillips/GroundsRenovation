###### 1. Change working directory to project folder
# setwd("~/GroundsRenovation")


###### 2. Libraries
library(lme4)

###### 3. Functions

source("R/GoogleSpreadsheets.R"))
source("R/DataFormat.R"))
source("R/SensitivityAnalysis.R"))


###### 4. Locations
## Incase figures are to be saved

# if(!dir.exists("Figures")){
#	dir.create("Figures")
# }
# figure_out <- "Figures"



###### 5. Model
## Needed to have saved the model previously
load("Models/FinalDensityModel.rda")


###### 6. Data
garden <- read.csv("data/Wildlife Garden - data collection - Sheet1.csv")
garden <- prepareGS(garden) ## Makes sure all factors are factors, and that all coordinates are in decimal degrees. NAs due to some sites not 



#### 7. Analysis

SA <- Sensitivity_Analysis(density3, reps = 1000) ## Model and the number of repititions
# pdf(file.path(figure_out, "SensitivityAnalysis.pdf"))
	hist(SA, xlab = "% Change", main="")
	abline(v=0)
# dev.off()
sum(SA < 0)/1000 * 100



