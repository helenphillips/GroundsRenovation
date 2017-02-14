###### 1. Change working directory to project folder
# setwd("~/GroundsRenovation")


###### 2. Libraries
library(lme4)

###### 3. Functions

source("R/GoogleSpreadsheets.R")
source("R/DataFormat.R")
source("R/SensitivityAnalysis.R")


###### 4. Locations
## Incase figures are to be saved

 if(!dir.exists("Figures")){
	dir.create("Figures")
 }
 figure_out <- "Figures"



###### 5. Model
## Needed to have saved the model previously
load("Models/FinalDensityModel.rda")
load("Models/FinalRichnessModel.rda")


###### 6. Data
garden <- read.csv("data/Wildlife Garden - data collection - Sheet1.csv")
garden <- prepareGS(garden) ## Makes sure all factors are factors, and that all coordinates are in decimal degrees. NAs due to some sites not 



#### 7. Analysis

SA_scaled <- Sensitivity_Analysis(density3, reps = 1000, scale.area = TRUE) ## Model and the number of repetitions
 pdf(file.path(figure_out, "SensitivityAnalysis_scaled.pdf"))
	hist(SA_scaled, xlab = "% Change", main="")
	abline(v=0)
 dev.off()
sum(SA_scaled < 0)/1000 * 100


SA_notscaled <- Sensitivity_Analysis(density3, reps = 1000, scale.area = FALSE) ## Model and the number of repetitions
sum(SA_notscaled < 0)/1000 * 100
SA_richness <- Sensitivity_Analysis(richness2, reps = 1000, scale.area = FALSE) ## Model and the number of repetitions
sum(SA_richness < 0)/1000 * 100


pdf(file.path(figure_out, "SensitivityAnalysis_notscaled.pdf"), width = 8)
  par(mfrow=c(1,2))
  par(mar=c(4, 4, 2, 0))
  hist(SA_notscaled, xlab = "", main="", ylab = "",  xlim = c(-0.5, 25), ylim=c(0,200), breaks = 20)
  # abline(v=0)
  mtext("(a)", side = 3, line = 0, at =0)
  hist(SA_richness, xlab = "", main="", ylab="", xlim = c(-0.5, 25), ylim=c(0,200))
  # abline(v=0)
  mtext("(b)", side = 3, line = 0, at =0)
  mtext('Frequency', side = 2, outer = TRUE, line = -1.5, las = 0, at = 0.55)
  mtext('% Change', side = 1, outer = TRUE, line = -1.5, las = 0)
dev.off()


## Alternative way to plot - which I don't like 
 
#hist(SA_richness, xlab = "% Change", main="", xlim = c(-0.5, 20), ylim = c(0, 300), col="darkgrey")
#hist(SA_notscaled, xlab = "% Change", main="", add=TRUE, breaks =20, col = "lightgrey")
#hist(SA_scaled, xlab = "% Change", main="", add=TRUE, breaks =20, col="white")



