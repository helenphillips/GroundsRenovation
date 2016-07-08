##############
## FUNCTIONS
##############
if(Sys.info()['nodename'] == "helensminimac.nhm.ac.uk"){
	path = "/Users/hp1111/PhD/git_phdChapters/git_WildlifeGarden"
}

source(file.path(path, "/PlantsDatabase/Scripts/Functions/RemoveDuplicates.R"))
source(file.path(path, "/PlantsDatabase/Scripts/Functions/SpeciesSimilarity.R"))
source(file.path(path, "/PlantsDatabase/Scripts/Functions/SpeciesOverTime.R"))
source(file.path(path, "/MetaAnalysis/Scripts/Functions/GoogleSpreadsheets.R"))

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

figure_out <- "~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/Figures"



##############
## DATA
##############
garden <- OpenGS()
## WHen not working
## garden <- read.csv("/Users/hp1111/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Data/Data_extract_2016-06-02.csv")
nhm <- garden[garden$Study.ID == "PlantDatabaseExtract",]

plants <- read.csv("~/Dropbox/PhD_Copy/Wildlife Garden/PlantsDatabase/DataFiles/PlantsByBed_NBN_Combined_2015-11-05-CMTR.csv")
plants$X <- NULL
nrow(plants) # 5731

plants <- plants[plants$NBN.TVK != "",]
nrow(plants) #5459 

names(plants)[1:2] <- c("ID", "Habitat")

levels(plants$Code)[levels(plants$Code) == "GR01"] <- "G01"
levels(plants$Code)[levels(plants$Code) == "GR02"] <- "G02"

### Adding in the NHM habitat classification
plants <- merge(plants, nhm, by.x="Code", by.y="Site.ID")[,c(1:10,15)]


## Check how many are duplicates in each area
l <- aggregate(plants$NBN.Name, list(plants$Code), function(x){length = length(x)})
u <- aggregate(plants$NBN.Name, list(plants$Code), function(x){unique = length(unique(x))})
areas <- cbind(l, u$x)


new_plants <- remove_duplicates(plants, columns = "Habitat.x")


## Check that length of each habitat is expected
#t <- as.data.frame(do.call(rbind, lapply(new_plants, function(x) nrow(x))))
#t$Group <- rownames(t)
#t <- t[order(t$Group),]
#areas <- cbind(areas, t$V1)
## Which it is




############################
## Number of Similar Species
############################

similarity_habitat <- species_similarity(new_plants)

png(file.path(figure_out, "HabitatSimiarity.png"), height = 700, width = 700)
levelplot(similarity_habitat, col.regions=colorRampPalette(c("white", "black")), scale=list(x=list(rot=45, cex=1.1), y=list(cex=1.1)), ylab = "", xlab = "",cex.txt=1.5)
dev.off()


new_plants_area <- remove_duplicates(plants, columns = "Code")
similarity_area <- species_similarity(new_plants_area)
png(file.path(figure_out, "AreaSimiarity.png"), height = 1000, width = 1000)
levelplot(similarity_area, col.regions=colorRampPalette(c("white", "black")), scale=list(x=list(rot=45)), ylab = "", xlab = "")
dev.off()

wanted_habitats <- c("ponds", "neutral grassland","marginal vegetation (pond edge)","short/perennial vegetation", "fen (incl. reedbed)", "chalk grassland", "species-rich hedgerow", "acid grassland (heath)", "broadleaved woodland", "hard standing","amenity grass/turf")
plants_NHMhabitats <- plants[plants$Habitat.y %in% wanted_habitats,]
new_plants_nhmhabitat <- remove_duplicates(plants_NHMhabitats, columns = "Habitat.y")
similarity_nhmhabitat <- species_similarity(new_plants_nhmhabitat)
labs <- attr(similarity_nhmhabitat, "dimnames")[[1]]

labs <-  c("Ponds", "Neutral Grassland", "Marginal Vegetation (pond edge)", "Short/Perennial Vegetation", "Fen (incl. Reedbed)", "Chalk Grassland", "Species-rich Hedgerow", "Acid Grassland (Heath)", "Broadleaved Woodland", "Hard Standing", "Amenity Grass/Turf")

attr(similarity_nhmhabitat, "dimnames")[[1]] <- labs
attr(similarity_nhmhabitat, "dimnames")[[2]] <- labs
png(file.path(figure_out, "NHMHabitatsSimiarity.png"))
levelplot(similarity_nhmhabitat, col.regions=colorRampPalette(c("white", "black")), scale=list(x=list(rot=45)), ylab = "", xlab = "")
dev.off()
############################
## Species over time
############################


y.axis_area <- seq(1, length(new_plants_area), 1)
x.axis <- c(1995, 2015)

Species_over_time(new_plants_area, x.axis, y.axis_areas)
y.axis <- seq(1, length(new_plants), 1)
Species_over_time(new_plants, x.axis, y.axis)


#############################
## Converting Codes to habitat types
#############################
## Using a previous extract for now
ext <- read.csv("~/Dropbox/PhD_Copy/Wildlife Garden/MetaAnalysis/Data/Data_extract_2015-12-03.csv")

wlg <- droplevels(ext[ext$Study.ID == "PlantDatabaseExtract",])

plants_habitat <- merge(plants, wlg, all.x=TRUE, by.x="Code", by.y = "Site.ID")[,c(1:10, 15)]

plants_habitat <- plants_habitat[!(is.na(plants_habitat$Habitat.y)),]
plants_habitat <- droplevels(plants_habitat[plants_habitat$Habitat.y != "Unsure/Not Clear",])


new_plants_habitat <- remove_duplicates(plants_habitat, columns = "Habitat.y")
similarity_habitat <- species_similarity(new_plants_habitat)
png(file.path(figure_out, "HabitatTypeSimiarity.png"), height = 500, width = 500)
levelplot(similarity_habitat, col.regions=colorRampPalette(c("white", "black")), scale=list(x=list(rot=45)), ylab = "Percent of X in common with Y", xlab = "Percent of X in common with Y")
dev.off()