###### 1. Change working directory to project folder
# setwd("~/GroundsRenovation")



##############
## FUNCTIONS
##############
source("R/RemoveDuplicates.R")
source("R/SpeciesSimilarity.R")
source("R/GoogleSpreadsheets.R")

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

##############
## libraries
##############

library(lattice)
library(ggplot2)

##############
## FOLDERS
##############

## Incase figures are to be saved

# if(!dir.exists("Figures")){
#	dir.create("Figures")
# }
# figure_out <- "Figures"


##############
## DATA
##############
garden <- read.csv("data/Wildlife Garden - data collection - Sheet1.csv")
garden <- prepareGS(garden) ## Makes sure all factors are factors, and that all coordinates are in decimal degrees. NAs due to some sites not have coordinates

## Just NHM sites, to match the areas with the Plant Database Extract
nhm <- garden[garden$Study.ID == "PlantDatabaseExtract",]

plants <- read.csv("data/PlantsByBed_NBN_Combined_2015-11-05-CMTR.csv")
plants$X <- NULL
nrow(plants) # 5731

plants <- plants[plants$NBN.TVK != "",]
nrow(plants) #5459 

names(plants)[1:2] <- c("ID", "Habitat")

## Incorrect codes
levels(plants$Code)[levels(plants$Code) == "GR01"] <- "G01"
levels(plants$Code)[levels(plants$Code) == "GR02"] <- "G02"

### Adding in the NHM habitat classification
plants <- merge(plants, nhm, by.x="Code", by.y="Site.ID")[,c(1:10,15)]

############################
## Species Similarity
############################

wanted_habitats <- c("ponds", "neutral grassland","marginal vegetation (pond edge)","short/perennial vegetation", "fen (incl. reedbed)", "chalk grassland", "species-rich hedgerow", "acid grassland (heath)", "broadleaved woodland", "hard standing","amenity grass/turf")
plants_NHMhabitats <- plants[plants$Habitat.y %in% wanted_habitats,]

## Remove duplicates of species within each habitat type (in this case, Habitat.y)
new_plants_nhmhabitat <- remove_duplicates(plants_NHMhabitats, columns = "Habitat.y")
## Calculate the similarity in species between each habitat type
similarity_nhmhabitat <- species_similarity(new_plants_nhmhabitat)
labs <- attr(similarity_nhmhabitat, "dimnames")[[1]]
labs <-  c("Ponds", "Neutral Grassland", "Marginal Vegetation (pond edge)", "Short/Perennial Vegetation", "Fen (incl. Reedbed)", "Chalk Grassland", "Species-rich Hedgerow", "Acid Grassland (Heath)", "Broadleaved Woodland", "Hard Standing", "Amenity Grass/Turf")
attr(similarity_nhmhabitat, "dimnames")[[1]] <- labs
attr(similarity_nhmhabitat, "dimnames")[[2]] <- labs

 pdf(file.path(figure_out, "NHMHabitatsSimiarity.pdf"), width = 8, height = 6)
levelplot(similarity_nhmhabitat, col.regions=colorRampPalette(c("white", "black")), scale=list(x=list(rot=45, cex=1.1), y=list(cex = 1.1)), 
            ylab = " ", xlab = " ")
 dev.off()

 
 