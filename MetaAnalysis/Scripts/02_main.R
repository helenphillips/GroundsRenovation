#################
## Functions
#################
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/GoogleSpreadsheets.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/CheckComparisons.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/DataFormat.R")
source("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Functions/ModelFunctions.R")
source("~/Dropbox/AdriannasFunctions/model_plot.R")

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
library(effect)

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

length(unique(garden$Study.ID)) ## 33

sources <- gsub("\\ [0-9]$", "", garden$Study.ID)
length(unique(sources)) ## 24



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
	text(-7, 49.5, "Figure 1 \nMap of study locations", pos=4)
dev.off()


#################
## Data Exploration
#################

## Converting to consistent units
garden$Sampled.Area_metres <- convertArea(garden$Sampled.Area, garden$Sampled.Area.Units, "sample")
garden$Habitat.Area_metres <- convertArea(garden$Habitat.Area, garden$Habitat.Area.Units, "habitat")


# "Flag" column so we know what has been converted to a species denisty
garden$SpeciesDensity <- FALSE

# When we know sampling area, coverting to species density per 10m2, and changing the "Flag"
garden$Corrected_Taxon.Richness <- ifelse(is.na(garden$Sampled.Area_metres), garden$Taxon.Richness, calculate_density_cSAR(S=garden$Taxon.Richness, A=garden$Sampled.Area_metres, z=0.1, newA = 10)) ## 
garden$SpeciesDensity <- ifelse(is.na(garden$Sampled.Area_metres), FALSE, TRUE)

## Calculate a species density for when an entire area has been sampled.
## As species density varies within fragment area
garden$Corrected_Taxon.Richness <- ifelse(is.na(garden$Sampled.Area_metres) & !(is.na(garden$Habitat.Area_metres)), calculate_density_cSAR(S=garden$Taxon.Richness, A=garden$Habitat.Area_metres, x=0.1, newA=10), garden$Corrected_Taxon.Richness)

garden$SpeciesDensity <- ifelse(is.na(garden$Sampled.Area_metres) & !(is.na(garden$Habitat.Area_metres)), TRUE, garden$SpeciesDensity)


levels(garden$Sampled.Area.Units)
not_areas <- c("Minutes (pond net)", "Minutes (sweep netting)")
garden$Corrected_Taxon.Richness <- ifelse(garden$Sampled.Area.Units %in% not_areas, garden$Taxon.Richness, garden$Corrected_Taxon.Richness) ## For those sampling efforts which are not areas, and therefore not following c-SAR
garden$SpeciesDensity <- ifelse(garden$Sampled.Area.Units %in% not_areas, FALSE, garden$SpeciesDensity) ## For those sampling efforts which are not areas, and therefore not following c-SAR


hist(garden$Corrected_Taxon.Richness)
garden$Study.ID[garden$Corrected_Taxon.Richness > 100] ## Because its cubic metres...

###########################################################
## SAMPLING AREA COMPARISONS
## Species Density models

#########
## Data
#########
sampled_area <- garden[garden$SpeciesDensity == TRUE,] ## 371
sampled_area <- sampled_area[sampled_area$Taxonimic.Level == "Species",] ## 367
sampled_area <- droplevels(sampled_area)
sampled_area <- sampled_area[complete.cases(sampled_area$Taxon.Richness),]## 345

table(sampled_area$Habitat, sampled_area$Study.ID)
table(sampled_area$Habitat)

not_enough <- c("green roof", "Not present in NHM", "species-poor hedgerow", "Unsure/Not Clear", "Unspecified grass/meadows", "orchard", "ferns and cycad plantings", "hard standing")

sampled_area <- sampled_area[!(sampled_area$Habitat %in% not_enough),] ## 327

sample_studies <- as.data.frame(aggregate(sampled_area$Habitat, list(sampled_area$Study.ID), function(x){N = length(unique(x, na.rm=TRUE))}))
sample_studies <- sample_studies[sample_studies$x > 1,]
## The studies with only one habitat in are amenity grasslands (pretty much), so not losing anything by removing

sampled_area <- sampled_area[sampled_area$Study.ID %in% sample_studies$Group.1,] ## 182
sampled_area <- droplevels(sampled_area)

table(sampled_area$Habitat)

tapply(sampled_area$Corrected_Taxon.Richness, sampled_area$Habitat, summary)

table(sampled_area$Study.ID, sampled_area$Habitat)

table(sampled_area$Study.ID, sampled_area$Taxa)

exclude_these_studies <- c("2003_Thompson 1", "2006_SmithA 1", "2006_SmithA 2")
sampled_area <- sampled_area[!(sampled_area$Study.ID %in% exclude_these_studies),] # 134
sampled_area <- droplevels(sampled_area) 

sampled_area$Habitat <- relevel(sampled_area$Habitat, ref = "broadleaved woodland")

#########
## Model
#########
taxa <- sampled_area$Taxa
levels(taxa)[levels(taxa) != "Plants"] <- "Inverts"
table(sampled_area$Habitat, taxa)
density <- round(sampled_area$Corrected_Taxon.Richness)

density1 <- glmer(density ~ Habitat * taxa + (1|Study.ID), data = sampled_area, family = poisson) ## Not convinved there is enough data for this model yet
density2 <- glmer(density ~ Habitat + taxa + (1|Study.ID), data = sampled_area, family = poisson)
density3 <- glmer(density ~ Habitat + (1|Study.ID), data = sampled_area, family = poisson)
anova(density2, density3)
summary(density3)
model_plot(density3) ## That's ok




density3_means <- model_Means(density3)
png(file.path(figure_out, "Habitat_density.png"), pointsize=11)
	labs <- levels(sampled_area$Habitat)
	par(mar=c(14, 4, 1, 1))
	errbar(1:nrow(density3_means), exp(density3_means[,2]), exp(density3_means[,3]), 
		exp(density3_means[,4]), col = "white", main = "", sub ="", xlab ="", bty = "n", 
		pch = 19, xaxt = "n", ylim=c(0,50), las = 1, cex= 1, ylab = "")
	points(1:nrow(density3_means),exp(density3_means[,2]),col="black",bg="white",pch=19,cex=1)
	axis(1, at=1:nrow(density3_means), labels = labs, las = 2)
	mtext(expression(Species ~ Density ~ (per ~ 10~m^{2})), side = 2, line = 2)
dev.off()


#############################################
### Other habitats
#############################################

shrubs <- read.csv("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Papers/1979_Strong/Data.csv")
trees <- shrubs$Species.Richness[shrubs$Host == "Trees" & shrubs$Status == "Middle"]
introduced_shrubs <- shrubs$Species.Richness[shrubs$Host == "Shrubs" & shrubs$Status == "Middle"]
introduced_shrubs_coef <- introduced_shrubs/trees
angiosperm_shrubs <- shrubs$Species.Richness[shrubs$Host == "Shrubs" & shrubs$Status == "Middle"]
angiosperm_shrubs_coef <- angiosperm_shrubs/trees

scriven <- garden[garden$Study.ID == "2013_Scriven 1",]
species_poor_hedge <- scriven$Taxon.Richness[scriven$Habitat == "species-poor hedgerow"]
species_rich_hedge <- scriven$Taxon.Richness[scriven$Habitat == "species-rich hedgerow"]
species_poor_hedge_coef <- species_poor_hedge/species_rich_hedge
#############################################
### BIODIVERSITY CHANGE
#############################################



habitat_areas <- data.frame(
Habitat = c("Broadleaved woodland", "Acid grassland (heath)", "Chalk grassland", "Neutral grassland", "Fen (incl. reedbed)", "Marginal vegetation (pond edge)", "Ponds", "Green roof", "Species-poor hedgerow", "Species-rich hedgerow", "Short/perennial vegetation", "Amenity grass/turf", "Introduced shrubs", "Hard standing", "Fern and cycad planting", "Agricultural plants", "Paleogene Asteraceae", "Neogene grass", "Cretaceous Angiosperm shrubs", "Total"),
Current_area_m2 = c(1978, 100, 425, 2050, 75, 190, 339, 9, 109, 77, 373.9, 3657, 2000, 9506, 0, 0, 0, 0, 0, NA),
Proposed_area_m2=c(3267, 82, 526, 2141, 134, 122, 460, 83, 0, 159, 736, 518, 1049, 9076, 760, 570, 177, 157, 245, NA)
)

habitat_areas$Habitat <- tolower(habitat_areas$Habitat)

tolower(density3_means$Habitat) %in% habitat_areas$Habitat
# Check this everytime
density3_means$Habitat <- tolower(density3_means$Habitat)
habitat_areas <- merge(habitat_areas, density3_means, by.x = "Habitat", by.y = "Habitat", all.x = TRUE)[,1:4]

names(habitat_areas)[4] <- "SpeciesDensity_10m2"
habitat_areas$SpeciesDensity_10m2 <- exp(habitat_areas$SpeciesDensity_10m2)

## Adding in other coefficients
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "introduced shrubs"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "broadleaved woodland"] * introduced_shrubs_coef
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "fern and cycad planting"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "broadleaved woodland"] * introduced_shrubs_coef
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "cretaceous angiosperm shrubs"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "broadleaved woodland"] * angiosperm_shrubs_coef
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "species-poor hedgerow"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "species-rich hedgerow"] * species_poor_hedge_coef
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "neogene grass"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "amenity grass/turf"]
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "paleogene asteraceae"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "short/perennial vegetation"]
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "hard standing"] <- 0

## Creating z values
habitat_areas$z <- 0.1
habitat_areas$DensityCorrectionFormula <- "exp(c*log(Area)^z)"


## Calculating the biodiversity change between the two
habitat_areas$Corrected_SpeciesDensity_Current <- calculate_density_cSAR(S=habitat_areas$SpeciesDensity_10m2, A=10, x=0.1, newA=habitat_areas$Current_area_m2)
habitat_areas$Corrected_SpeciesDensity_Proposed <- calculate_density_cSAR(S=habitat_areas$SpeciesDensity_10m2, A=10, x=0.1, newA=habitat_areas$Proposed_area_m2)


### Area weights

totalCurrent <- sum(habitat_areas$Current_area_m2, na.rm = TRUE)
totalProposed <- sum(habitat_areas$Proposed_area_m2, na.rm = TRUE)

habitat_areas$Current_weight <- habitat_areas$Current_area_m2/totalCurrent
habitat_areas$Current_Weighted_density <- habitat_areas$SpeciesDensity_10m2 * habitat_areas$Current_weight
habitat_areas$Current_Weighted_Correcteddensity <- habitat_areas$Corrected_SpeciesDensity_Current * habitat_areas$Current_weight

habitat_areas$Proposed_weight <- habitat_areas$Proposed_area_m2/totalProposed
habitat_areas$Proposed_Weighted_density <- habitat_areas$SpeciesDensity_10m2 * habitat_areas$Proposed_weight
habitat_areas$Proposed_Weighted_Correcteddensity <- habitat_areas$Corrected_SpeciesDensity_Proposed * habitat_areas$Proposed_weight

## Totals
habitat_areas[20,2:ncol(habitat_areas)] <- NA
habitat_areas[20, c(2:3, 10:15)] <- colSums(habitat_areas[,c(2:3, 10:15)], na.rm = TRUE)

write.csv(habitat_areas, file = "~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Scripts/Table/Habitat_totals.csv", row.names = FALSE)


#############################################
### PLOTS WITH ALL COEFFICIENTS
#############################################
png(file.path(figure_out, "Habitat_density_plusmissing.png"), pointsize=11)
	missing_coefs <- c("cretaceous angiosperm shrubs", "fern and cycad planting", "hard standing", 
		"introduced shrubs", "neogene grass", "paleogene asteraceae", "species-poor hedgerow")
	labs <- levels(sampled_area$Habitat)
	labs2 <- c(labs, missing_coefs)
	par(mar=c(14, 4, 1, 1))
	errbar(1:nrow(density3_means), exp(density3_means[,2]), exp(density3_means[,3]), exp(density3_means[,4]), 
		col = "white", main = "", sub ="", xlab ="", bty = "n", pch = 19, xaxt = "n", ylim=c(0,50), las = 1, cex= 1, ylab = "", xlim=c(0, length(labs2)))
	points(1:nrow(density3_means),exp(density3_means[,2]),col="black",bg="white",pch=19,cex=1)
	missing_densities <- habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat %in% missing_coefs]
	points((nrow(density3_means)+1):(length(labs2)), missing_densities, col="red", pch=19)
	axis(1, at=1:length(labs2), labels = labs2, las = 2)
	mtext(expression(Species ~ Density ~ (per ~ 10~m^{2})), side = 2, line = 2)
dev.off()
