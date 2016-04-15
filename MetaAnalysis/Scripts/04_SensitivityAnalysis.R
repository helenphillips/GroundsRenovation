###### 1. Change working directory to "MetaAnalysis" folder

setwd("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis")

###### 2. Libraries
library(lme4)
source("Scripts/Functions/GoogleSpreadsheets.R")
source("Scripts/Functions/DataFormat.R")
source("Scripts/Functions/SensitivityAnalysis.R")


###### 3. Model
load("Models/FinalDensityModel.rda")
garden <- OpenGS()



#### 4. New dataframe
figure_out <- "Scripts/Figures"



SA <- Sensitivity_Analysis(density3, reps = 1000)
png(file.path(figure_out, "SensitivityAnalysis.png"))
hist(SA, xlab = "% Change", main="")
abline(v=0)
dev.off()
sum(SA < 0)/1000 * 100



