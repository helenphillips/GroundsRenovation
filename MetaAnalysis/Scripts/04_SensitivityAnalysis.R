###### 1. Change working directory to "MetaAnalysis" folder

setwd("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis")

###### 2. Libraries
library(lme4)
source("Scripts/Functions/GoogleSpreadsheets.R")
source("Scripts/Functions/DataFormat.R")

###### 3. Model
load("Models/FinalDensityModel.rda")
garden <- OpenGS()



#### 4. New dataframe

SA <- Sensitivity_Analysis(density3, reps = 1000)
hist(SA)
sum(SA < 0)/1000 * 100



