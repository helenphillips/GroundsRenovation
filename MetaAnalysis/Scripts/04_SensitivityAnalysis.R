###### 1. Change working directory to "MetaAnalysis" folder

setwd("~/Box Sync/help/PhD_Copy/Wildlife Garden/MetaAnalysis")

###### 2. Libraries
library(lme4)
source("/Users/helenphillips/PhD_git/WildlifeGarden_chapter/MetaAnalysis/Scripts/Functions/GoogleSpreadsheets.R")
source("/Users/helenphillips/PhD_git/WildlifeGarden_chapter/MetaAnalysis/Scripts/Functions/DataFormat.R")
source("/Users/helenphillips/PhD_git/WildlifeGarden_chapter/MetaAnalysis/Scripts/Functions/SensitivityAnalysis.R")


###### 3. Model
load("Models/FinalDensityModel.rda")
garden <- OpenGS()



#### 4. New dataframe
figure_out <- "Figures"



SA <- Sensitivity_Analysis(density3, reps = 1000)
png(file.path(figure_out, "SensitivityAnalysis.png"))
hist(SA, xlab = "% Change", main="")
abline(v=0)
dev.off()
sum(SA < 0)/1000 * 100



