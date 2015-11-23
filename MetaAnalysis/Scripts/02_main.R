#################
## Functions
#################
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/GoogleSpreadsheets.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/CheckComparisons.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/DataFormat.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/ModelFunctions.R")


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
library(Hmisc)

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

length(unique(garden$Study.ID)) ## 31

sources <- gsub("\\ [0-9]$", "", garden$Study.ID)
length(unique(sources)) ## 22



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

## Correcting sampling area
garden$Total_sampledArea <- garden$Sampled.Area * garden$Number.of.samples
garden$Total_sampledArea_metres <- convertArea(garden$Total_sampledArea, garden$Sampled.Area.Units, "sample")

garden$Corrected_Taxon.Richness <- ifelse(is.na(garden$Total_sampledArea_metres), garden$Taxon.Richness, garden$Taxon.Richness/garden$Total_sampledArea_metres) ## Will need to change this when we have a slope for sampled area vs. richness

hist(garden$Corrected_Taxon.Richness)
garden$Study.ID[garden$Corrected_Taxon.Richness > 100] ## Because its cubic metres...

temp <-garden[!(garden$Study.ID %in% c("2006_SmithA 1", "2006_SmithA 2")),]
hist(temp$Corrected_Taxon.Richness) ## That's better, so for now, exclude those two Smith studies	


###########################################################
## HABITAT COMPARISONS

#########
## Data
#########
## Only want studies with more than one habitat in
studies <- as.data.frame(aggregate(garden$Habitat, list(garden$Study.ID), function(x){N = length(unique(x, na.rm=TRUE))}))
studies <- studies[studies$x > 1,]
habitat <- garden[garden$Study.ID %in% studies$Group.1,] ## 176 rows
habitat <- habitat[!(habitat$Study.ID %in% c("2006_SmithA 1", "2006_SmithA 2")),] # 208 rows

habitat <- habitat[complete.cases(habitat$Corrected_Taxon.Richness),] ## 165
table(habitat$Habitat)
table(habitat$Taxonimic.Level)

habitat <- habitat[habitat$Taxonimic.Level == "Species",] # 161
habitat <- droplevels(habitat)

hist(habitat$Corrected_Taxon.Richness)

any(habitat$Study.ID %in% sampled_effort_varies) ## At the moment, not

#################
## Models
#################

Richness <- round(habitat$Corrected_Taxon.Richness)

habitat1 <- glmer(Richness ~ Habitat + (1|Study.ID) + (1|Taxa), family = poisson, data = habitat)
summary(habitat1) # reference = acid grassland (heath)

habitat1_means <- model_Means(habitat1)

png(file.path(figure_out, "Habitat_means.png"), pointsize=11)
labs <- levels(habitat$Habitat)
par(mar=c(10, 4, 1, 1))
errbar(1:nrow(habitat1_means), exp(habitat1_means[,2]), exp(habitat1_means[,3]), exp(habitat1_means[,4]), col = "white", main = "", sub ="", xlab ="", bty = "n", pch = 19, xaxt = "n", ylim=c(0,40), las = 1, cex= 1, ylab = "")
points(1:nrow(habitat1_means),exp(habitat1_means[,2]),col="black",bg="white",pch=19,cex=1)
axis(1, at=1:nrow(habitat1_means), labels = labs, las = 2)
mtext(expression(Species ~ Density ~ (per ~ m^{2})), side = 2, line = 2)
dev.off()

###########################################################
## HABITAT AREA COMPARISONS

#########
## Data
#########

area <- aggregate(garden$Habitat.Area, list(garden$Study.ID), function(x){var_area = var(x, na.rm=TRUE)})
studies_area <- area$Group.1[area$x > 0]

areas <- garden[garden$Study.ID %in% studies_area,]


hist(areas$Taxon.Richness) ## ok

table(areas$Taxonimic.Level)
areas <- areas[areas$Taxonimic.Level == "Species",]
## Don't want studies where sampled area != habitat area

sampled_area <- aggregate(areas$Total_sampledArea_metres, list(areas$Study.ID), function(x){var_area = mean(x, na.rm=TRUE)})
studied_exclude <- sampled_area$Group.1[!is.na(sampled_area$x)]
others <- c("2015_Noble 2", "2005_Leather 1","2008_Hartley 2")
studied_exclude <- factor(c(as.character(studied_exclude),as.character(others)))

areas <- areas[!(areas$Study.ID %in% studied_exclude),]
areas <- droplevels(areas)
nrow(areas) ## 145

hist(areas$Taxon.Richness) ## Excellent


# Units
levels(areas$Habitat.Area.Units)
areas$Habitat.Area_ha <- convertArea(areas$Habitat.Area, areas$Habitat.Area.Units, type = c("habitat"))

habs <- c("amenity grass/turf","broadleaved woodland","fen (incl. reedbed)","marginal vegetation (pond edge)","ponds","short/perennial vegetation","species-rich hedgerow")               

areas <- areas[areas$Habitat %in% habs,]
nrow(areas) #128

areas <- droplevels(areas)

tapply(areas$Habitat.Area_ha, areas$Habitat, summary)
tapply(areas$Taxon.Richness, areas$Habitat, summary)


pdf(file.path(figure_out, "AreaRichness.pdf"))
for(h in 1:length(habs)){
	plot(areas$Taxon.Richness[areas$Habitat == habs[h]] ~ areas$Habitat.Area_ha[areas$Habitat == habs[h]], col = areas$Study.ID, main = habs[h])
}
dev.off()

areas$Habitat <- relevel(areas$Habitat, ref = "ponds")

#################
## Models
#################
richness <- round(areas$Taxon.Richness)
area1 <- glmer(richness ~ Habitat.Area_ha * Habitat + (1|Study.ID), family = poisson, data = areas)
area2 <- glm(richness  ~ Habitat.Area_ha * Habitat + Study.ID, family = poisson, data = areas)

Anova(area1)




