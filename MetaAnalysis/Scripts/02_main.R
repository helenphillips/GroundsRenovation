#################
## Functions
#################
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/GoogleSpreadsheets.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/CheckComparisons.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/DataFormat.R")

#################
## Libraries
#################

library(lme4)
library(maps)
library(mapdata)
library(maptools)
library(MuMIn)
library(dplyr)
library(car)

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

x <- aggregate(garden$Number.of.samples, list(garden$Study.ID), function(x){var_effort = var(x, na.rm=TRUE)})
sampled_effort_varies <- x$Group.1[x$x > 0]

y <- aggregate(garden$Sampled.Area, list(garden$Study.ID), function(x){var_area = var(x, na.rm=TRUE)})

sampled_area_varies <- y$Group.1[y$x > 0]


###########################################################
## HABITAT COMPARISONS

#########
## Data
#########

studies <- as.data.frame(aggregate(garden$Habitat, list(garden$Study.ID), function(x){N = length(unique(x, na.rm=TRUE))}))
studies <- studies[studies$x > 1,]
habitat <- garden[garden$Study.ID %in% studies$Group.1,] ## 176 rows

habitat <- habitat[complete.cases(habitat$Taxon.Richness),] ## 135
table(habitat$Habitat)
table(habitat$Taxonimic.Level)

habitat <- habitat[habitat$Taxonimic.Level == "Species",]


habitat <- droplevels(habitat)

hist(habitat$Taxon.Richness)
unique(habitat$Study.ID[habitat$Taxon.Richness > 100]) ## 2003_Thompson 1

any(habitat$Study.ID %in% sampled_effort_varies) ## At the moment, not
# unique(habitat$Study.ID[which(habitat$Study.ID %in% sampled_effort_varies)])

### Sampled area is now the issue
table(habitat$Sampled.Area.Units, habitat$Study.ID)

tapply(habitat$Sampled.Area, habitat$Study.ID, max)

## This code is weak - I have one stud that is cm3, the rest are m2
## This line will not work if there are any other units
habitat$SampledArea_metres <- ifelse(habitat$Sampled.Area.Units == "m2", habitat$Sampled.Area, habitat$Sampled.Area*1e-6)

#################
## Models
#################



###########################################################
## HABITAT AREA COMPARISONS

#########
## Data
#########

area <- aggregate(garden$Habitat.Area, list(garden$Study.ID), function(x){var_area = var(x, na.rm=TRUE)})
studies_area <- area$Group.1[area$x > 0]

areas <- garden[garden$Study.ID %in% studies_area,]


hist(areas$Taxon.Richness) ## Ooooo looks nice

table(areas$Taxonimic.Level)
areas <- areas[areas$Taxonimic.Level == "Species",]
## Variation in effort or sampled area??

any(areas$Study.ID %in% sampled_area_varies)
areas <- droplevels(areas[areas$Study.ID != "2006_Kadas 1",])


table(areas$Habitat.Area.Units)
areas

table(areas$Habitat, areas$Study.ID)




#################
## Models
#################
richness <- round(areas$Taxon.Richness)
area1 <- glmer(richness ~ Habitat.Area * Habitat + (1|Study.ID), family = poisson, data = areas)
Anova(area1)




