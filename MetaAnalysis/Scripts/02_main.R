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

studied_include <- c("2000_Linton 1", "2015_Sirohi 1")

areas <- areas[areas$Study.ID %in% studied_include,]
areas <- droplevels(areas)
nrow(areas) ## 81

hist(areas$Taxon.Richness) ## Excellent


# Units
levels(areas$Habitat.Area.Units)
areas$Habitat.Area_ha <- convertArea(areas$Habitat.Area, areas$Habitat.Area.Units, type = c("habitat"))

habs <- c("amenity grass/turf","broadleaved woodland","ponds","short/perennial vegetation")               

areas <- areas[areas$Habitat %in% habs,]
nrow(areas) #77

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
area1 <- glm(richness ~ Habitat.Area_ha * Habitat, family = poisson, data = areas)

par(mfrow=c(2,2))
plot(area1)

Anova(area1)

## Three level habitat
habitats <- areas$Habitat
levels(habitats)[levels(habitats) == "short/perennial vegetation"] <- "non-woodlands"
levels(habitats)[levels(habitats) == "amenity grass/turf"] <- "non-woodlands"

area2 <- glm(richness ~ Habitat.Area_ha * habitats, family = poisson, data = areas)

anova(area1, area2, test = "Chisq") ## Not significant

## Two level habitat
habitats2 <- areas$Habitat
levels(habitats2)[levels(habitats2) == "short/perennial vegetation"] <- "terrestrial"
levels(habitats2)[levels(habitats2) == "amenity grass/turf"] <- "terrestrial"
levels(habitats2)[levels(habitats2) == "broadleaved woodland"] <- "terrestrial"

area3 <- glm(richness ~ Habitat.Area_ha * habitats2, family = poisson, data = areas)
anova(area2, area3, test = "Chisq") ## Not significant

## Just to check, but as expected, significantly different
area4 <- glm(richness ~ Habitat.Area_ha, family = poisson, data = areas)
anova(area3, area4, test = "Chisq")

######## Model check

par(mfrow=c(2,2))
plot(area3) ## Not bad at all


### Plotting
newdat <- expand.grid(Habitat.Area_ha = seq(min(areas$Habitat.Area_ha), max(areas$Habitat.Area_ha), 0.1), habitats2 = c("terrestrial", "ponds"))
to_plot_terrestrial <- predict(area3, newdata = newdat[newdat$habitats2 == "terrestrial",], type ="response")
to_plot_ponds <- predict(area3, newdata = newdat[newdat$habitats2 == "ponds",], type ="response")


area_cols <- areas$Habitat
levels(area_cols)[levels(area_cols) == "ponds"] <- "blue" 
levels(area_cols)[levels(area_cols) == "amenity grass/turf"] <- "red" 
levels(area_cols)[levels(area_cols) == "broadleaved woodland"] <- "green" 
levels(area_cols)[levels(area_cols) == "short/perennial vegetation"] <- "yellow" 
area_cols <- as.character(area_cols)

plot(to_plot_ponds ~ Habitat.Area_ha, newdat_ponds, type = "l", col = "blue", lwd = 2, ylab = "Species Richness", xlab = "Habitat Area (hectares)")
lines(seq(min(areas$Habitat.Area_ha), max(areas$Habitat.Area_ha), 0.1), to_plot_terrestrial, col = "darkgreen", lwd = 2)
points(jitter(areas$Taxon.Richness) ~ jitter(areas$Habitat.Area_ha), pch = 19, col = area_cols, cex = 0.7)

