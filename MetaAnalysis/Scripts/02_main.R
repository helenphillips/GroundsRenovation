#################
## Functions
#################
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/GoogleSpreadsheets.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/CheckComparisons.R")

#################
## Libraries
#################

library(lme4)
library(maps)
library(mapdata)
library(maptools)
library(MuMIn)
library(dplyr)


#################
## Locations
#################

figure_out <- "~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Figures"

#################
## Data
#################

garden <- OpenGS()


################################

comparisons <- check_comparisons(garden)
pdf(file = "~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Figures/HabitatComparisons.pdf")
comparisons[[2]]
dev.off()

#################
## Study info 
#################

length(unique(garden$Study.ID)) ## 28

sources <- gsub("\\ [0-9]$", "", garden$Study.ID)
length(unique(sources)) ## 19



#################
## MAP
#################
coord<-aggregate(cbind(garden$long, garden$lat), list(garden$Study.ID), max)
coord$X<-coord$Group.1
coord<-coord[2:4]
names(coord)<-c("Long", "Lat", "X")
dsSPDF<-SpatialPointsDataFrame(coord[,1:2], data.frame(coord[,1:3]))
proj4string(dsSPDF)<-CRS("+proj=longlat")

png(file.path(figure_out, "Map.png"), pointsize=11)
par(mar=c(0, 0, 0, 0))
map('worldHires', c("UK"),border=0,fill=TRUE, col="forestgreen",  xlim=c(-8,2), ylim=c(49,60.9), mar = c(0, 0, 0, 0))
points(dsSPDF, col="black", bg="black", cex= 1.5, pch=19)
text(-7, 49.5, "Figure 1 \nMap of the study locations", pos=4)
dev.off()


#################
## Data Exploration
#################

hist(garden$Taxon.Richness)
garden$Study.ID[garden$Taxon.Richness > 100] ## Because its multiple samples


###########################################################
## HABITAT COMPARISONS

sampled_effort_varies <- c("2015_Smith 1", "2005_Leather 1")
sampled_area_varies <- c("2006_Kadas 1")

#########
## Data
#########

studies <- as.data.frame(aggregate(garden$Habitat, list(garden$Study.ID), function(x){N = length(unique(x, na.rm=TRUE))}))
studies <- studies[studies$x > 1,]
habitat <- garden[garden$Study.ID %in% studies$Group.1,] ## 176 rows

habitat <- habitat[complete.cases(habitat$Taxon.Richness),] ## 135
table(habitat$Habitat)

habitat <- droplevels(habitat)

hist(habitat$Taxon.Richness)

any(habitat$Study.ID %in% sampled_effort_varies)
unique(habitat$Study.ID[which(habitat$Study.ID %in% sampled_effort_varies)])

#################
## Models
#################






